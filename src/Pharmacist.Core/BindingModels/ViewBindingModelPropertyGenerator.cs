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
            var viewForParameter = TypeParameter("TView");
            var viewModelParameter = TypeParameter("TViewModel");
#pragma warning disable SA1129 // Do not use default value type constructor
            var sep = new SeparatedSyntaxList<TypeParameterSyntax>();
#pragma warning restore SA1129 // Do not use default value type constructor
            sep = sep.AddRange(new[] { viewForParameter, viewModelParameter });
            var typeParameterList = TypeParameterList(sep);

            var controlClassFullName = orderedTypeDeclaration.typeDefinition.FullName;

            var modifiers = TokenList(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.SealedKeyword));

#pragma warning disable SA1129 // Do not use default value type constructor
            var viewConstraints = new SeparatedSyntaxList<TypeParameterConstraintSyntax>();
#pragma warning restore SA1129 // Do not use default value type constructor
            var viewForConstraint = SyntaxFactory.TypeConstraint(SyntaxFactory.ParseTypeName("ReactiveUI.IViewFor<TViewModel>"));

            viewConstraints = viewConstraints
                .Add(SyntaxFactory.ClassOrStructConstraint(SyntaxKind.ClassConstraint))
                .Add(viewForConstraint);
            var viewConstraintClause = SyntaxFactory.TypeParameterConstraintClause(
                SyntaxFactory.IdentifierName("TView"),
                viewConstraints);

            var reactiveObjectInterfaceConstraint = SyntaxFactory.TypeConstraint(SyntaxFactory.ParseTypeName("ReactiveUI.IReactiveObject"));
#pragma warning disable SA1129 // Do not use default value type constructor
            var viewModelConstraints = new SeparatedSyntaxList<TypeParameterConstraintSyntax>();
#pragma warning restore SA1129 // Do not use default value type constructor
            viewModelConstraints =
                viewModelConstraints
                    .Add(SyntaxFactory.ClassOrStructConstraint(SyntaxKind.ClassConstraint))
                    .Add(reactiveObjectInterfaceConstraint);
            var viewModelConstraintClause = SyntaxFactory.TypeParameterConstraintClause(
                SyntaxFactory.IdentifierName("TViewModel"),
                viewModelConstraints);
            var constraintClauses = new SyntaxList<TypeParameterConstraintClauseSyntax>(new[] { viewConstraintClause, viewModelConstraintClause });

            return ClassDeclaration($"{orderedTypeDeclaration.typeDefinition.Name}ViewBindingModel")
                .WithModifiers(modifiers)
                .WithTypeParameterList(typeParameterList)
                .WithConstraintClauses(constraintClauses)
                .WithLeadingTrivia(XmlSyntaxFactory.GenerateSummarySeeAlsoComment("A class that contains View Bindings for the {0} control.", $"global::{controlClassFullName}"))
                .WithMembers(GetProperties(controlClassFullName, orderedTypeDeclaration.properties));
        }

        private static SyntaxList<MemberDeclarationSyntax> GetProperties(
            string controlClassFullName,
            IEnumerable<IProperty> properties)
        {
            var propArray = properties.ToArray();
            var nodes = new List<MemberDeclarationSyntax>(propArray.Length);

            foreach (var prop in propArray)
            {
                var accessorList = new[]
                {
                    SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                        .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken)),
                    SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)
                        .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                };

                var summary = XmlSyntaxFactory.GenerateSummarySeeAlsoComment(
                    "Gets or sets the binding logic for {0}",
                    $"global::{controlClassFullName}.{prop.Name}");

                var propSyntax = GetPropertyDeclaration(prop, accessorList, summary);

                nodes.Add(propSyntax);
            }

            return new SyntaxList<MemberDeclarationSyntax>(nodes);
        }

        private static PropertyDeclarationSyntax GetPropertyDeclaration(
            IProperty prop,
            AccessorDeclarationSyntax[] accessorList,
            IEnumerable<SyntaxTrivia> summary)
        {
            var bindingType = prop.CanSet ? "Two" : "One";

            var type = ParseTypeName($"ReactiveUI.Core.ViewBindingModels.I{bindingType}WayBind<TView, global::{prop.ReturnType.FullName}>");

            var result = PropertyDeclaration(type, prop.Name)
                .AddModifiers(Token(SyntaxKind.PublicKeyword))
                .WithAccessorList(
                    AccessorList(List(accessorList)))
                .WithLeadingTrivia(summary);

            return result;
        }
    }
}
