using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.Operations;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading.Tasks;

namespace ClassLibrary3
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(CodeFix))]
    public class CodeFix : CodeFixProvider
    {
        public override ImmutableArray<string> FixableDiagnosticIds { get; } = ImmutableArray.Create("LG1000");

        public override FixAllProvider GetFixAllProvider() => null;

        public override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var document = context.Document;
            var root = await context.Document.GetSyntaxRootAsync();
            var semanticModel = await context.Document.GetSemanticModelAsync();

            foreach (var diagnostic in context.Diagnostics)
            {
                var invocationLoc = diagnostic.AdditionalLocations[0];
                var assignmentLoc = diagnostic.AdditionalLocations[1];

                var assignment = (AssignmentExpressionSyntax)root.FindToken(diagnostic.Location.SourceSpan.Start)
                    .Parent.FirstAncestorOrSelf<AssignmentExpressionSyntax>();

                var invocation = (InvocationExpressionSyntax)assignment.Right;

                var field = (IFieldSymbol)(semanticModel.GetSymbolInfo(assignment.Left).Symbol);

                context.RegisterCodeFix(CodeAction.Create("Use LoggerMessageAttribute", async (cts) =>
                {
                    var result = await SymbolFinder.FindReferencesAsync(field, document.Project.Solution, ImmutableHashSet.Create(document));

                    MethodDeclarationSyntax loggerMethod = null;
                    foreach (var location in result.SelectMany(r => r.Locations))
                    {
                        var node = root.FindNode(location.Location.SourceSpan);
                        loggerMethod = node?.FirstAncestorOrSelf<MethodDeclarationSyntax>();
                        if (loggerMethod != null)
                        {
                            break;
                        }
                    }

                    var shizzle = false;
                    if (loggerMethod?.Body is null || loggerMethod.Body.Statements.Count > 1)
                    {
                        // It's doing some processing. Ignore.
                        shizzle = true;
                    }

                    var argList = SyntaxFactory.SeparatedList<AttributeArgumentSyntax>();

                    var operation = (IInvocationOperation)semanticModel.GetOperation(invocation);

                    var logLevel = ((ArgumentSyntax)operation.Arguments.First(f => f.Parameter.Name == "logLevel").Syntax).Expression;
                    var eventIdObject = (ObjectCreationExpressionSyntax)((ArgumentSyntax)operation.Arguments.First(f => f.Parameter.Name == "eventId").Syntax).Expression;
                    var eventId = eventIdObject.ArgumentList.Arguments[0].Expression;
                    var eventName = eventIdObject.ArgumentList.Arguments[1].Expression;
                    var message = ((ArgumentSyntax)operation.Arguments.First(f => f.Parameter.Name == "formatString").Syntax).Expression;

                    argList = argList.AddRange(
                        new[]
                        {
                            SyntaxFactory.AttributeArgument(eventId),
                            SyntaxFactory.AttributeArgument(logLevel),
                            SyntaxFactory.AttributeArgument(message),
                            SyntaxFactory.AttributeArgument(
                                SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName("EventName")),
                                nameColon: null,
                                expression: eventName),
                        });

                    var skipEnabledCheck = operation.Arguments.FirstOrDefault(f => f.Parameter.Name == "skipEnabledCheck");

                    if (skipEnabledCheck != null)
                    {
                        argList = argList.Add(SyntaxFactory.AttributeArgument(
                                SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName("SkipEnabledCheck")),
                                nameColon: null,
                                expression: ((ArgumentSyntax)skipEnabledCheck.Syntax).Expression));
                    }

                    var attributeArgs = SyntaxFactory.AttributeArgumentList(argList);
                    var attribute = SyntaxFactory.Attribute(SyntaxFactory.ParseName("LoggerMessage"), attributeArgs);

                    var solution = document.Project.Solution;
                    SolutionEditor solutionEditor = new SolutionEditor(solution);
                    var editor = await solutionEditor.GetDocumentEditorAsync(document.Id);
                    editor.RemoveNode(assignment.Parent);
                    editor.RemoveNode(field.DeclaringSyntaxReferences.First().GetSyntax());

                    var updated = loggerMethod
                            .AddAttributeLists(SyntaxFactory.AttributeList(SyntaxFactory.SeparatedList(new[] { attribute })))
                                .WithLeadingTrivia(SyntaxFactory.SyntaxTrivia(SyntaxKind.WhitespaceTrivia, Environment.NewLine))
                            .AddModifiers(SyntaxFactory.Token(SyntaxKind.PartialKeyword))
                            .WithBody(null)
                            .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken));


                    var loggerParameter = updated.ParameterList.Parameters[0];
                    updated = updated.ReplaceNode(loggerParameter, loggerParameter.WithModifiers(SyntaxFactory.TokenList()));

                    var loggerMethodSymbol = semanticModel.GetDeclaredSymbol(loggerMethod);
                    var callers = await SymbolFinder.FindCallersAsync(loggerMethodSymbol, document.Project.Solution);

                    var logTypes = new Dictionary<DocumentId, TypeDeclarationSyntax>();

                    foreach (var instance in callers)
                    {
                        var syntax = instance.CallingSymbol.DeclaringSyntaxReferences.First().GetSyntax();
                        foreach (var location in instance.Locations)
                        {
                            var callerDocument = solution.GetDocument(syntax.SyntaxTree);
                            var callerDocumentEditor = await solutionEditor.GetDocumentEditorAsync(callerDocument.Id);
                            var caller = callerDocumentEditor.GetChangedRoot().FindNode(location.SourceSpan).FirstAncestorOrSelf<InvocationExpressionSyntax>();
                            var memberAccess = (MemberAccessExpressionSyntax)caller.Expression;

                            var callerArgs = caller.ArgumentList.Arguments
                                .Insert(0, SyntaxFactory.Argument(memberAccess.Expression));

                            var rewritten = caller;
                            callerDocumentEditor.ReplaceNode(caller, (d, s) => ((InvocationExpressionSyntax)d).WithExpression(memberAccess.WithExpression(SyntaxFactory.IdentifierName("Log")))
                                .WithArgumentList(SyntaxFactory.ArgumentList(callerArgs)));

                            var type = caller.FirstAncestorOrSelf<TypeDeclarationSyntax>();
                            var logType = type.Members.OfType<TypeDeclarationSyntax>().FirstOrDefault(f => f.Identifier.ToFullString().Trim().Contains("Log"));

                            if (logType != null)
                            {
                                callerDocumentEditor.AddMember(logType, updated.WithLeadingTrivia(updated.GetLeadingTrivia().Insert(0, SyntaxFactory.CarriageReturnLineFeed)));
                            }
                            else
                            {
                                if (!logTypes.TryGetValue(callerDocument.Id, out logType))
                                {
                                    logType = SyntaxFactory.TypeDeclaration(SyntaxKind.ClassDeclaration, "Log")
                                        .AddModifiers(SyntaxFactory.Token(SyntaxKind.PrivateKeyword), SyntaxFactory.Token(SyntaxKind.PartialKeyword));
                                    logTypes[callerDocument.Id] = logType;
                                }

                                logTypes[callerDocument.Id] = logType.AddMembers(updated);
                            }
                        }
                    }

                    foreach (var item in logTypes)
                    {
                        var callerDocumentEditor = await solutionEditor.GetDocumentEditorAsync(item.Key);
                        var type = callerDocumentEditor.OriginalRoot.DescendantNodes().OfType<TypeDeclarationSyntax>().First();
                        callerDocumentEditor.AddMember(type, item.Value);
                        if (!type.Modifiers.Any(m => m.ValueText == "partial"))
                        {
                            callerDocumentEditor.ReplaceNode(type, (d, g) => ((TypeDeclarationSyntax)d).AddModifiers(SyntaxFactory.Token(SyntaxKind.PartialKeyword)));
                        }
                    }

                    if (!shizzle)
                        editor.RemoveNode(loggerMethod);

                    return solutionEditor.GetChangedSolution();
                }, equivalenceKey: "Logger"), diagnostic);
            }
        }
    }
}
