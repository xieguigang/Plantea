Imports Microsoft.VisualBasic.Language
Imports Microsoft.VisualBasic.Linq
Imports PlantToolKit
Imports SMRUCC.Rsharp.Runtime
Imports SMRUCC.Rsharp.Runtime.Components
Imports SMRUCC.Rsharp.Runtime.Internal.Object
Imports SMRUCC.Rsharp.Runtime.Interop

Module Tools

    Public Function pullNetwork(<RRawVectorArgument> regulations As Object, Optional env As Environment = Nothing) As [Variant](Of Message, IEnumerable(Of RegulationFootprint))
        Dim pull As IEnumerable(Of RegulationFootprint)

        If TypeOf regulations Is list Then
            pull = DirectCast(regulations, list).data _
                .Select(Function(a)
                            Dim part = pipeline.TryCreatePipeline(Of RegulationFootprint)(a, env)

                            If part.isError Then
                                Return {}
                            Else
                                Return part.populates(Of RegulationFootprint)(env)
                            End If
                        End Function) _
                .IteratesALL
        Else
            With pipeline.TryCreatePipeline(Of RegulationFootprint)(regulations, env)
                If .isError Then
                    Return .getError
                Else
                    pull = .populates(Of RegulationFootprint)(env)
                End If
            End With
        End If

        Return New [Variant](Of Message, IEnumerable(Of RegulationFootprint))(pull)
    End Function
End Module
