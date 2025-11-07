Imports Microsoft.VisualBasic.ApplicationServices.Terminal.ProgressBar.Tqdm
Imports Microsoft.VisualBasic.CommandLine.Reflection
Imports Microsoft.VisualBasic.Linq
Imports Microsoft.VisualBasic.Scripting.MetaData
Imports Microsoft.VisualBasic.Scripting.Runtime
Imports Microsoft.VisualBasic.Text.Xml.Models
Imports SMRUCC.genomics.Analysis.SequenceTools.SequencePatterns.Motif
Imports SMRUCC.genomics.ComponentModel.Annotation
Imports SMRUCC.Rsharp.Runtime
Imports SMRUCC.Rsharp.Runtime.Internal.Object
Imports SMRUCC.Rsharp.Runtime.Interop
Imports SMRUCC.Rsharp.Runtime.Vectorization
Imports Rdataframe = SMRUCC.Rsharp.Runtime.Internal.Object.dataframe
Imports RInternal = SMRUCC.Rsharp.Runtime.Internal

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
        Call df.add("gene_id", From id As MotifLink In list Select id.Gene_id.JoinBy("; "))
        Call df.add("species", From id As MotifLink In list Select id.Species)
        Call df.add("method", From id As MotifLink In list Select id.Method)
        Call df.add("data_source", From id As MotifLink In list Select id.Datasource)
        Call df.add("datasource_id", From id As MotifLink In list Select id.Datasource_ID)

        Return df
    End Function

    ''' <summary>
    ''' load motif database from a given xml list dataset
    ''' </summary>
    ''' <returns></returns>
    <ExportAPI("load_motifdb")>
    Public Function loadMotifDb(file As String) As MotifPWM()
        Return file.LoadXml(Of XmlList(Of MotifPWM))() _
            .AsEnumerable _
            .ToArray
    End Function

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="motifLinks"></param>
    ''' <param name="motif_hits"></param>
    ''' <param name="regulators">
    ''' should be a blast alignment result of the subclass of <see cref="IQueryHits"/>.
    ''' </param>
    ''' <param name="env"></param>
    ''' <returns></returns>
    <ExportAPI("tf_network")>
    <RApiReturn(GetType(RegulationFootprint))>
    Public Function LinkTFNetwork(motifLinks As MotifLink(), motif_hits As MotifMatch(),
                                  <RRawVectorArgument>
                                  regulators As Object,
                                  sourceMaps As list,
                                  Optional env As Environment = Nothing) As Object

        Dim regs As pipeline = pipeline.TryCreatePipeline(Of IQueryHits)(regulators, env)

        If regs.isError Then
            Return regs.getError
        End If

        Dim matrixIndex As Dictionary(Of String, MotifLink()) = motifLinks _
            .GroupBy(Function(a) a.Matrix_id) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.ToArray
                          End Function)
        Dim regulatorHits As Dictionary(Of String, IQueryHits()) = regs.populates(Of IQueryHits)(env) _
            .GroupBy(Function(a) a.hitName) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.ToArray
                          End Function)
        Dim network As New List(Of RegulationFootprint)
        Dim regMaps As Dictionary(Of String, String()) = sourceMaps.slots _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return CLRVector.asCharacter(a.Value)
                          End Function)

        For Each scan As MotifMatch In TqdmWrapper.Wrap(motif_hits)
            Dim motif_seed = scan.seeds(0).Split.Skip(1).ToArray
            Dim links = matrixIndex(motif_seed.First)
            Dim gene_ids As String() = links.Select(Function(l) l.Gene_id).IteratesALL.ToArray
            Dim regList As New List(Of IQueryHits)

            For Each source_id As String In gene_ids
                If regulatorHits.ContainsKey(source_id) Then
                    Call regList.AddRange(regulatorHits(source_id))
                ElseIf regMaps.ContainsKey(source_id) Then
                    For Each mappedId As String In regMaps(source_id)
                        Call regList.AddRange(regulatorHits(mappedId))
                    Next
                Else
                    Call $"missing regulator mapping information for motif hit: {scan}".warning
                End If
            Next

            Dim target_meta As String() = scan.title.Split("|"c)
            Dim reg_desc = regList.Select(Function(r) r.description.Split("|"c)(1)).Distinct.ToArray
            Dim region = target_meta(2).Split("-"c).AsInteger

            Call network.Add(New RegulationFootprint With {
                .chromosome = target_meta(0),
                .Sequence = scan.segment,
                .MotifId = motif_seed.First,
                .Signature = scan.motif,
                .tag = scan.seeds(0),
                .RegulatorTrace = regList.Select(Function(a) a.hitName).Distinct.JoinBy("; "),
                .Regulator = regList.Select(Function(a) a.queryName).Distinct.JoinBy("; "),
                .ORF = target_meta(1),
                .MotifFamily = reg_desc.JoinBy(", "),
                .MotifTrace = scan.seeds.First,
                .Distance = -scan.start,
                .pvalue = scan.pvalue
            })
        Next

        Return network.ToArray
    End Function
End Module
