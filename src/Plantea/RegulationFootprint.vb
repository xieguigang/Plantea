Imports SMRUCC.genomics.Model.Network.VirtualFootprint.DocumentFormat

''' <summary>
''' The transcription regulation network edge
''' </summary>
Public Class RegulationFootprint : Inherits RegulatesFootprints

    Public Property chromosome As String
    Public Property pvalue As Double

    Public Property regulator_group As String
    Public Property target_group As String

End Class
