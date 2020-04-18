// Copyright (c) 2019 .NET Foundation and Contributors. All rights reserved.
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ICSharpCode.Decompiler.TypeSystem;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pharmacist.Core.Generation;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Pharmacist.Core.BindingModels
{
    /// <summary>
    /// Generates a ViewBindingModel for the discovered types.
    /// </summary>
    public sealed class ViewBindingModelPropertyGenerator : IPropertyGenerator
    {
        /// <inheritdoc/>
        public IEnumerable<NamespaceDeclarationSyntax> Generate(IEnumerable<(ITypeDefinition typeDefinition, ITypeDefinition? baseDefinition, IEnumerable<IProperty> properties)> values)
        {
            foreach (var groupedDeclarations in values.GroupBy(x => x.typeDefinition.Namespace).OrderBy(x => x.Key))
            {
                var namespaceName = $"ReactiveUi.ViewBindingModels.{groupedDeclarations.Key}";
                var members = new List<ClassDeclarationSyntax>();

                var orderedTypeDeclarations = groupedDeclarations.OrderBy(x => x.typeDefinition.Name).ToList();

                foreach (var orderedTypeDeclaration in orderedTypeDeclarations)
                {
                    members.Add(GenerateClass(namespaceName, orderedTypeDeclaration));
                }

                if (members.Count > 0)
                {
                    yield return NamespaceDeclaration(IdentifierName(namespaceName))
                        .WithMembers(List<MemberDeclarationSyntax>(members));
                }
            }
        }

        private static ClassDeclarationSyntax GenerateClass(
            string namespaceName,
            (ITypeDefinition typeDefinition, ITypeDefinition? baseDefinition, IEnumerable<IProperty> properties) orderedTypeDeclaration)
        {
            return ClassDeclaration($"{orderedTypeDeclaration.typeDefinition.Name}ViewBindingModel")
                .WithModifiers(TokenList(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.SealedKeyword)))
                .WithLeadingTrivia(XmlSyntaxFactory.GenerateSummarySeeAlsoComment("A class that contains View Bindings for the {0} control.", orderedTypeDeclaration.typeDefinition.FullName));
        }
    }
}
