Imports Microsoft.VisualBasic.ComponentModel.DataSourceModel
Imports Microsoft.VisualBasic.Serialization.JSON

Public Class TFInfo

    Public Property protein_id As String
    Public Property species As String
    Public Property family As String
    Public Property description As String

    Public ReadOnly Property gene_id As String
        Get
            Return protein_id
        End Get
    End Property

    Sub New()
    End Sub

    Sub New(title As String)
        Dim t As String() = title.Split("|"c)
        Dim mol As NamedValue(Of String) = t(0).GetTagValue(" ")

        protein_id = mol.Name
        species = mol.Value
        family = t(1)
        description = t.Skip(2).JoinBy("|")
    End Sub

    Public Overrides Function ToString() As String
        Return Me.GetJson
    End Function

End Class
