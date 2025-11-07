Imports Microsoft.VisualBasic.Data.Framework.StorageProvider.Reflection

Public Class MotifLink

    Public Property Matrix_id As String

    ''' <summary>
    ''' A set of the gene id that associated with current motif object
    ''' </summary>
    ''' <returns></returns>
    <Collection("Gene_id", ", ")>
    Public Property Gene_id As String()

    Public Property Species As String
    Public Property Method As String
    Public Property Datasource As String
    Public Property Datasource_ID As String

    Public Overrides Function ToString() As String
        Return Matrix_id
    End Function

End Class
