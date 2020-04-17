// Copyright (c) 2019 .NET Foundation and Contributors. All rights reserved.
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Text;
using ICSharpCode.Decompiler.TypeSystem;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pharmacist.Core.Generation.Resolvers;

namespace Pharmacist.Core.BindingModels
{
    /// <summary>
    /// Generates the Binding Models for a namespace.
    /// </summary>
    public class BindingModelNamespaceResolver : INamespaceResolver
    {
        /// <inheritdoc/>
        public IEnumerable<NamespaceDeclarationSyntax> Create(ICompilation compilation)
        {
            throw new NotImplementedException();
        }
    }
}
