// Copyright (c) 2019 .NET Foundation and Contributors. All rights reserved.
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using ICSharpCode.Decompiler.TypeSystem;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pharmacist.Core.Generation;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using TypeKind = ICSharpCode.Decompiler.TypeSystem.TypeKind;

namespace Pharmacist.Core.BindingModels
{
    /// <summary>
    /// Generates a ViewBinding Helper for the discovered types.
    /// </summary>
    public sealed class ViewBindingHelperGenerator : IPropertyGenerator
    {
        /// <inheritdoc/>
        public IEnumerable<NamespaceDeclarationSyntax> Generate(IEnumerable<(ITypeDefinition typeDefinition, ITypeDefinition? baseDefinition, IEnumerable<IProperty> properties)> values)
        {
            foreach (var groupedDeclarations in values.GroupBy(x => x.typeDefinition.Namespace).OrderBy(x => x.Key))
            {
                var namespaceName = $"ReactiveUI.ViewBindingHelpers.{groupedDeclarations.Key}";
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

            var modifiers = TokenList(Token(SyntaxKind.PublicKeyword));

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

            var td = orderedTypeDeclaration.typeDefinition;

            var classDeclaration = ClassDeclaration($"{td.Name}ViewBindingHelper");

            var isDerivedFromBaseElement =
                !td.FullName.Equals("System.Windows.UIElement", StringComparison.OrdinalIgnoreCase);

            if (isDerivedFromBaseElement)
            {
                var baseClass = td.DirectBaseTypes.FirstOrDefault(x => x.Kind == TypeKind.Class);

                var baseViewBindingModelClassName = $"global::ReactiveUI.ViewBindingHelpers.{baseClass.FullName}ViewBindingHelper<TView>";
                var baseTypeNode =
                    SyntaxFactory.SimpleBaseType(SyntaxFactory.ParseTypeName(baseViewBindingModelClassName));
#pragma warning disable SA1129 // Do not use default value type constructor
                var baseTypesList = new SeparatedSyntaxList<BaseTypeSyntax>();
#pragma warning restore SA1129 // Do not use default value type constructor
                baseTypesList = baseTypesList.Add(baseTypeNode);
                var baseList = BaseList(baseTypesList);

                classDeclaration = classDeclaration.WithBaseList(baseList);
            }

            return classDeclaration
                .WithModifiers(modifiers)
                .WithTypeParameterList(typeParameterList)
                .WithConstraintClauses(constraintClauses)
                .WithLeadingTrivia(XmlSyntaxFactory.GenerateSummarySeeAlsoComment("A class that helps carry out View Bindings for the {0} control.", $"global::{controlClassFullName}"))
                .WithMembers(GetMethods(isDerivedFromBaseElement, orderedTypeDeclaration.properties));
        }

        private static SyntaxList<MemberDeclarationSyntax> GetMethods(
            bool isDerivedFromBaseElement,
            IEnumerable<IProperty> properties)
        {
            var propArray = properties.ToArray();
            var nodes = new List<MemberDeclarationSyntax>(propArray.Length);

            var returnType = ParseTypeName("void");

            var body = Block();

            const string methodName = "ApplyBindings";

            if (isDerivedFromBaseElement)
            {
                // need to call the base class to call relevant binding helpers
                var fieldMemberAccess = MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName("base"),
                    IdentifierName(methodName));

                var baseInvocation = InvocationExpression(fieldMemberAccess);
                var statement = ExpressionStatement(baseInvocation);
                body = body.AddStatements(statement);
            }

            body = ApplyPropertyInvocations(body, properties);

            var modifiers = TokenList(Token(SyntaxKind.PublicKeyword));

            var method = MethodDeclaration(returnType, methodName)
                .WithModifiers(modifiers)
                .WithBody(body);

            nodes.Add(method);

            return new SyntaxList<MemberDeclarationSyntax>(nodes);
        }

        private static BlockSyntax ApplyPropertyInvocations(BlockSyntax body, IEnumerable<IProperty> properties)
        {
            foreach (var prop in properties)
            {
                var fieldMemberAccess = MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName("global::ReactiveUI.ViewBindingHelpers.BaseBindingHelpers"),
                    IdentifierName("ApplyBinding"));

                var propertyNameArg = Argument(IdentifierName(prop.Name));
#pragma warning disable SA1129 // Do not use default value type constructor
                var arguments = new SeparatedSyntaxList<ArgumentSyntax>();
#pragma warning restore SA1129 // Do not use default value type constructor
                arguments = arguments.Add(propertyNameArg);
                var argumentList = ArgumentList(arguments);

                var baseInvocation = InvocationExpression(fieldMemberAccess, argumentList);
                var statement = ExpressionStatement(baseInvocation);
                body = body.AddStatements(statement);
            }

            return body;
        }
    }
}
