Imports System.Text.RegularExpressions
Imports Microsoft.VisualBasic.ComponentModel.DataSourceModel
Imports Microsoft.VisualBasic.Serialization.JSON

Public Class TFInfo

    Public Property protein_id As String
    Public Property species As String
    Public Property family As String
    Public Property description As String

    Public ReadOnly Property gene_id As String
        Get
            Dim prot_id As String = protein_id

            prot_id = RemoveTrailingIntegerSuffix(prot_id)

            Return prot_id
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

    Public Shared Function RemoveTrailingIntegerSuffix(inputString As String) As String
        ' 正则表达式模式：匹配以点开头后接一个或多个数字，并且位于字符串末尾的部分
        Dim pattern As String = "\.\d+$"
        ' 使用 Regex.Replace 方法将匹配到的部分替换为空字符串
        Dim result As String = Regex.Replace(inputString, pattern, String.Empty)

        Return result
    End Function

End Class
