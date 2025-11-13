Imports Microsoft.VisualBasic.Linq
Imports Microsoft.VisualBasic.Text.Xml.Models
Imports SMRUCC.genomics.Analysis.HTS.GSEA
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

    Public Overrides Function ToString() As String
        Return $"[{[class]}] {gene}"
    End Function

    Public Shared Function BuildBackground(geneSet As IEnumerable(Of ClassClusterData)) As Background
        Return New Background With {
            .clusters = geneSet.SafeQuery _
                .GroupBy(Function(gene) gene.class) _
                .Select(Function(cl) CreateCluster(cl.Key, cl)) _
                .ToArray,
            .size = .clusters.BackgroundSize
        }
    End Function

    Private Shared Function CreateCluster(label As String, list As IEnumerable(Of ClassClusterData)) As Cluster
        Return New Cluster With {
            .ID = label,
            .names = .ID,
            .members = CreateGeneObjects(list).ToArray
        }
    End Function

    Private Shared Iterator Function CreateGeneObjects(list As IEnumerable(Of ClassClusterData)) As IEnumerable(Of BackgroundGene)
        For Each gene As ClassClusterData In list
            Yield New BackgroundGene With {
                .accessionID = gene.gene,
                .name = .accessionID,
                .locus_tag = New NamedValue(.accessionID)
            }
        Next
    End Function

End Class