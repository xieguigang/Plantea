Imports Microsoft.VisualBasic.Scripting.MetaData
Imports SMRUCC.Rsharp.Runtime.Interop
Imports RInternal = SMRUCC.Rsharp.Runtime.Internal
Imports Rdataframe = SMRUCC.Rsharp.Runtime.Internal.Object.dataframe
Imports SMRUCC.Rsharp.Runtime
Imports SMRUCC.Rsharp.Runtime.Internal.Object

<Package("Plantea")>
<RTypeExport("motif_link", GetType(MotifLink))>
Module Exports

    Sub Main()
        Call RInternal.Object.Converts.makeDataframe.addHandler(GetType(MotifLink()), AddressOf castMotifLinktable)
    End Sub

    <RGenericOverloads("as.data.frame")>
    Public Function castMotifLinktable(list As MotifLink(), args As list, env As Environment) As Rdataframe
        Dim df As New Rdataframe With {
            .columns = New Dictionary(Of String, Array)
        }

        Call df.add("matrix_id", From id As MotifLink In list Select id.Matrix_id)
        Call df.add("gene_id", From id As MotifLink In list Select id.Gene_id)
        Call df.add("species", From id As MotifLink In list Select id.Species)
        Call df.add("method", From id As MotifLink In list Select id.Method)
        Call df.add("data_source", From id As MotifLink In list Select id.Datasource)
        Call df.add("datasource_id", From id As MotifLink In list Select id.Datasource_ID)

        Return df
    End Function
End Module
