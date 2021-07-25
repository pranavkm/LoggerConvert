using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis;
using System;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.Operations;
using System.Threading;

namespace ClassLibrary3
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class TestAnalyzer : DiagnosticAnalyzer
    {
#pragma warning disable RS2008 // Enable analyzer release tracking
        private static readonly DiagnosticDescriptor diagnosticDescriptor = new DiagnosticDescriptor("LG1000", "LoggerMessage", "Use LoggerMessageAttribute", "Performance", DiagnosticSeverity.Error, true);
#pragma warning restore RS2008 // Enable analyzer release tracking

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(diagnosticDescriptor);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);

            context.RegisterCompilationStartAction(compilation =>
            {
                var loggerMessage = compilation.Compilation.GetTypeByMetadataName("Microsoft.Extensions.Logging.LoggerMessage");

                compilation.RegisterOperationAction(symbolContext =>
                {
                    var invocation = (IInvocationOperation)symbolContext.Operation;
                    if (invocation.TargetMethod.Name == "Define" &&
                        SymbolEqualityComparer.Default.Equals(invocation.TargetMethod.ContainingType, loggerMessage))
                    {
                        var parent = invocation.Parent;
                        while (parent != null & !(parent is IAssignmentOperation))
                        {
                            parent = parent.Parent;
                        }

                        var lhs = ((IAssignmentOperation)parent).Target;

                        symbolContext.ReportDiagnostic(Diagnostic.Create(diagnosticDescriptor, 
                            parent.Syntax.GetLocation(), 
                            additionalLocations: new[] { invocation.Syntax.GetLocation(), lhs.Syntax.GetLocation() }));
                    }

                }, OperationKind.Invocation);
            });
        }
    }
}
