Imports Microsoft.VisualBasic.Linq
Imports SMRUCC.genomics.Model.Network.VirtualFootprint.DocumentFormat

''' <summary>
''' The transcription regulation network edge
''' </summary>
Public Class RegulationFootprint : Inherits RegulatesFootprints

    Public Property chromosome As String
    Public Property pvalue As Double

    Public Property regulator_group As String
    Public Property target_group As String

    Public Shared Iterator Function AssignClassData(regs As IEnumerable(Of RegulationFootprint), kb As Dictionary(Of String, ClassClusterData)) As IEnumerable(Of RegulationFootprint)
        For Each link As RegulationFootprint In regs.SafeQuery
            Dim term As String = If(link.target_group, "")

            If kb.ContainsKey(term) Then
                With kb(term)
                    link.class = .class
                    link.subclass = .subclass
                End With
            End If

            Yield link
        Next
    End Function

End Class

Public Class ClassClusterData

    Public Property gene As String
    Public Property [class] As String
    Public Property subclass As String

End Class