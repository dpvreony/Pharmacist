// Copyright (c) 2019 .NET Foundation and Contributors. All rights reserved.
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Metadata;
using System.Text;
using System.Threading.Tasks;
using ICSharpCode.Decompiler.TypeSystem;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pharmacist.Core.Generation;
using Pharmacist.Core.Generation.Resolvers;
using Pharmacist.Core.Utilities;

namespace Pharmacist.Core.BindingModels
{
    /// <summary>
    /// Generates the Binding Models for a namespace.
    /// </summary>
    public class BindingModelNamespaceResolver : INamespaceResolver
    {
        /// <inheritdoc />
        public IEnumerable<NamespaceDeclarationSyntax> Create(ICompilation compilation)
        {
            var typesAndEvents = GetValidBindingModelDetails(compilation);

            // need to look at this in long run as bit crude
            // could create a collection to pass in and add to.
            // 1) extract the namespace logic
            // 2) create input list sized as number of generators * namespace count.
            var models = GetBindingModelGenerator().Generate(typesAndEvents);
            var helpers = GetBindingHelperGenerator().Generate(typesAndEvents);

            return models.Concat(helpers);
        }

        private static bool IsValidProperty(IProperty x) => x.Accessibility == Accessibility.Public
                                                            && !x.IsExplicitInterfaceImplementation
                                                            && !x.IsStatic;

        private static IEnumerable<ITypeDefinition> GetPublicControls(ICompilation compilation)
        {
            return compilation.GetPublicTypeClassesWithDerivedType();
        }

        private static ITypeDefinition? GetValidBaseType(ITypeDefinition typeDefinition, ICompilation compilation)
        {
            var processedTypes = new HashSet<ITypeDefinition>();
            var processingQueue = new Queue<IType>(typeDefinition.DirectBaseTypes);

            while (processingQueue.Count != 0)
            {
                var currentType = processingQueue.Dequeue().GetRealType(compilation).GetDefinition();

                if (currentType == null || currentType.Kind == TypeKind.Class || currentType.Kind == TypeKind.TypeParameter)
                {
                    continue;
                }

                if (processedTypes.Contains(currentType))
                {
                    continue;
                }

                processedTypes.Add(currentType);

                processingQueue.EnqueueRange(currentType.DirectBaseTypes);
            }

            return null;
        }

        private IPropertyGenerator GetBindingModelGenerator()
        {
            return new ViewBindingModelPropertyGenerator();
        }

        private IPropertyGenerator GetBindingHelperGenerator()
        {
            return new ViewBindingHelperGenerator();
        }

        private IEnumerable<(
            ITypeDefinition typeHostingEvent,
            ITypeDefinition? baseTypeDefinition,
            IEnumerable<IProperty> events)> GetValidBindingModelDetails(ICompilation compilation)
        {
            var processedList = new ConcurrentDictionary<ITypeDefinition, bool>(TypeDefinitionNameComparer.Default);
            var toProcess = new ConcurrentStack<ITypeDefinition>(GetPublicControls(compilation));
            var output = new ConcurrentBag<(ITypeDefinition typeHostingEvent, ITypeDefinition? baseTypeDefinition, IEnumerable<IProperty> events)>();

            var processing = new ITypeDefinition[Environment.ProcessorCount];
            while (!toProcess.IsEmpty)
            {
                var count = toProcess.TryPopRange(processing);

                var processingList = processing.Take(count);
                Parallel.ForEach(
                    processingList,
                    typeDefinition =>
                    {
                        if (!processedList.TryAdd(typeDefinition, true))
                        {
                            return;
                        }

                        var validEvents = new HashSet<IProperty>();

                        foreach (var currentProperty in typeDefinition.Properties)
                        {
                            if (!IsValidProperty(currentProperty))
                            {
                                continue;
                            }

                            validEvents.Add(currentProperty);
                        }

                        var baseType = GetValidBaseType(typeDefinition, compilation);

                        if (baseType != null)
                        {
                            toProcess.Push(baseType);
                        }

                        output.Add((typeDefinition, baseType, validEvents));
                    });
            }

            return output;
        }
    }
}
