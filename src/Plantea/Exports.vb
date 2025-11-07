Imports Microsoft.VisualBasic.ApplicationServices.Terminal.ProgressBar.Tqdm
Imports Microsoft.VisualBasic.CommandLine.Reflection
Imports Microsoft.VisualBasic.Data.Framework
Imports Microsoft.VisualBasic.Linq
Imports Microsoft.VisualBasic.Scripting.MetaData
Imports Microsoft.VisualBasic.Scripting.Runtime
Imports Microsoft.VisualBasic.Text.Xml.Models
Imports SMRUCC.genomics.Analysis.SequenceTools.SequencePatterns.Motif
Imports SMRUCC.genomics.ComponentModel.Annotation
Imports SMRUCC.genomics.SequenceModel.FASTA
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

    <ExportAPI("extract_tf_info")>
    <RApiReturn(GetType(TFInfo))>
    Public Function extractTFInfo(<RRawVectorArgument> TF_fsa As Object, Optional env As Environment = Nothing) As Object
        Dim pull As pipeline = pipeline.TryCreatePipeline(Of FastaSeq)(TF_fsa, env)

        If pull.isError Then
            Return pull.getError
        Else
            Return pull.populates(Of FastaSeq)(env) _
                .Select(Function(fa) New TFInfo(fa.Title)) _
                .ToArray
        End If
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
                                  Optional env As Environment = Nothing) As Object

        Dim regs As pipeline = pipeline.TryCreatePipeline(Of IQueryHits)(regulators, env)

        If regs.isError Then
            Return regs.getError
        End If

        Dim TFdb = env.globalEnvironment _
            .GetResourceFile("data/PlantTFDB/TF.csv", package:="Plantea") _
            .LoadCsv(Of TFInfo)(mute:=True) _
            .ToArray

        Dim TFfamily As Dictionary(Of String, TFInfo()) = TFdb _
            .GroupBy(Function(tf) tf.family) _
            .ToDictionary(Function(tf) tf.Key,
                          Function(tf)
                              Return tf.ToArray
                          End Function)
        Dim TFGeneIndex As Dictionary(Of String, TFInfo()) = TFdb _
            .GroupBy(Function(a) a.gene_id) _
            .OrderByDescending(Function(a) a.Count) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.ToArray
                          End Function)
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
        Dim regMaps As New Dictionary(Of String, String()) ' = sourceMaps.slots _
        '    .ToDictionary(Function(a) a.Key,
        '                  Function(a)
        '                      Return CLRVector.asCharacter(a.Value)
        '                  End Function)
        Dim missing As New List(Of String)

        For Each scan As MotifMatch In TqdmWrapper.Wrap(motif_hits)
            Dim motif_seed = scan.seeds(0).Split.Skip(1).ToArray
            Dim links = matrixIndex(motif_seed.First)
            Dim gene_ids As String() = links.Select(Function(l) l.Gene_id).IteratesALL.ToArray
            Dim regList As New List(Of IQueryHits)
            Dim infertype As String = "missing"

            For Each source_id As String In gene_ids
                If regulatorHits.ContainsKey(source_id) Then
                    Call regList.AddRange(regulatorHits(source_id))
                    infertype = "conserved"
                ElseIf regMaps.ContainsKey(source_id) Then
                    For Each mappedId As String In regMaps(source_id)
                        Call regList.AddRange(regulatorHits(mappedId))
                    Next
                    infertype = "conserved"
                ElseIf TFGeneIndex.ContainsKey(source_id) Then
                    Dim maps = TFGeneIndex(source_id)

                    infertype = "family propagate"

                    For Each map As TFInfo In maps
                        Dim infer = TFfamily(map.family)
                        Dim anyhits As IQueryHits() = infer _
                            .Where(Function(a) regulatorHits.ContainsKey(a.protein_id)) _
                            .Select(Function(a) regulatorHits(a.protein_id)) _
                            .IteratesALL _
                            .ToArray

                        Call regList.AddRange(anyhits)
                    Next

                    If regList.Any Then
                        regList = New List(Of IQueryHits) From {
                            regList _
                                .OrderByDescending(Function(a) a.identities) _
                                .First
                        }
                    End If
                Else

                End If
            Next

            Dim target_meta As String() = scan.title.Split("|"c)
            Dim reg_desc = regList.Select(Function(r) r.description.Split("|"c)(1)).Distinct.ToArray
            Dim region = target_meta(2).Split("-"c).AsInteger

            Call network.Add(New RegulationFootprint With {
                .chromosome = target_meta(0),
                .sequence = scan.segment,
                .motif_id = motif_seed.First,
                .signature = scan.motif,
                .tag = scan.seeds(0),
                .regulator_trace = regList.Select(Function(a) a.hitName).Distinct.JoinBy("; "),
                .regulator = regList.Select(Function(a) a.queryName).Distinct.JoinBy("; "),
                .ORF = target_meta(1),
                .motif_family = reg_desc.JoinBy(", "),
                .motif_trace = scan.seeds.First,
                .distance = -scan.start,
                .pvalue = scan.pvalue,
                .type = infertype
            })
        Next

        Return network.ToArray
    End Function
End Module
