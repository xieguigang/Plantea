Imports Microsoft.VisualBasic.ApplicationServices.Terminal.ProgressBar.Tqdm
Imports Microsoft.VisualBasic.CommandLine.Reflection
Imports Microsoft.VisualBasic.Data.Framework
Imports Microsoft.VisualBasic.Data.Framework.IO
Imports Microsoft.VisualBasic.Data.visualize.Network.Graph
Imports Microsoft.VisualBasic.Language
Imports Microsoft.VisualBasic.Linq
Imports Microsoft.VisualBasic.Scripting.MetaData
Imports Microsoft.VisualBasic.Scripting.Runtime
Imports Microsoft.VisualBasic.Text.Xml.Models
Imports SMRUCC.genomics.Analysis.SequenceTools.SequencePatterns.Motif
Imports SMRUCC.genomics.ComponentModel.Annotation
Imports SMRUCC.genomics.Interops.NCBI.Extensions.Pipeline
Imports SMRUCC.genomics.SequenceModel.FASTA
Imports SMRUCC.Rsharp.Runtime
Imports SMRUCC.Rsharp.Runtime.Components
Imports SMRUCC.Rsharp.Runtime.Internal.Object
Imports SMRUCC.Rsharp.Runtime.Interop
Imports Rdataframe = SMRUCC.Rsharp.Runtime.Internal.Object.dataframe
Imports RInternal = SMRUCC.Rsharp.Runtime.Internal

''' <summary>
''' The plant genomics data analysis tools
''' </summary>
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
    ''' A helper function extract fo the PlantTFDB information
    ''' </summary>
    ''' <param name="TF_fsa"></param>
    ''' <param name="env"></param>
    ''' <returns></returns>
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
    ''' read regulation network from a given csv table file
    ''' </summary>
    ''' <param name="file"></param>
    ''' <returns></returns>
    <ExportAPI("read_regulation")>
    Public Function readRegulations(file As String) As RegulationFootprint()
        Return file.LoadCsv(Of RegulationFootprint)(mute:=True).ToArray
    End Function

    ''' <summary>
    ''' create subnetwork by matches a set of terms
    ''' </summary>
    ''' <param name="regulations"></param>
    ''' <param name="terms"></param>
    ''' <param name="env"></param>
    ''' <returns></returns>
    <ExportAPI("term_subnetwork")>
    <RApiReturn(GetType(RegulationFootprint))>
    Public Function subnetwork(<RRawVectorArgument> regulations As Object, <RRawVectorArgument> terms As Object, Optional env As Environment = Nothing) As Object
        Dim pulldata = pullNetwork(regulations, env)
        Dim rankTerms As pipeline = pipeline.TryCreatePipeline(Of RankTerm)(terms, env)

        If pulldata Like GetType(Message) Then
            Return pulldata.TryCast(Of Message)
        End If

        Dim termsIndex As Dictionary(Of String, RankTerm) = rankTerms.populates(Of RankTerm)(env).ToDictionary(Function(a) a.queryName)
        Dim subnet As New List(Of RegulationFootprint)

        For Each prot_id As String In termsIndex.Keys.ToArray
            Dim gene_id As String = prot_id.Split("."c).First

            If gene_id <> prot_id Then
                Call termsIndex.Add(gene_id, termsIndex(prot_id))
            End If
        Next

        For Each link As RegulationFootprint In pulldata.TryCast(Of IEnumerable(Of RegulationFootprint))
            Dim hit As Boolean = False

            If termsIndex.ContainsKey(link.ORF) Then
                hit = True
                link.target_group = termsIndex(link.ORF).term
            End If
            If link.regulator IsNot Nothing AndAlso termsIndex.ContainsKey(link.regulator) Then
                hit = True
                link.regulator_group = termsIndex(link.regulator).term
            End If

            If hit Then
                Call subnet.Add(link)
            End If
        Next

        Return subnet.ToArray
    End Function

    Private Function pullNetwork(<RRawVectorArgument> regulations As Object, Optional env As Environment = Nothing) As [Variant](Of Message, IEnumerable(Of RegulationFootprint))
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

    <ExportAPI("count_matrix")>
    Public Function embedding_matrix(<RRawVectorArgument> regulations As Object, Optional env As Environment = Nothing) As Object
        Dim gene_hits As New Dictionary(Of String, DataSet)
        Dim tag As String
        Dim pulldata = pullNetwork(regulations, env)

        If pulldata Like GetType(Message) Then
            Return pulldata.TryCast(Of Message)
        End If

        For Each link As RegulationFootprint In pulldata.TryCast(Of IEnumerable(Of RegulationFootprint))
            If Not gene_hits.ContainsKey(link.ORF) Then
                Call gene_hits.Add(link.ORF, New DataSet With {
                     .ID = link.ORF,
                     .Properties = New Dictionary(Of String, Double)
                })
            End If

            tag = If(link.motif_family.StringEmpty(, True),
                "missing",
                link.motif_family)
            gene_hits(link.ORF)(tag) = gene_hits(link.ORF)(tag) + 1
        Next

        Return gene_hits.Values.ToArray
    End Function

    <ExportAPI("as.regulation_graph")>
    <RApiReturn(GetType(NetworkGraph))>
    Public Function createGraph(<RRawVectorArgument> regulations As Object, Optional env As Environment = Nothing) As Object
        Dim pulldata = pullNetwork(regulations, env)

        If pulldata Like GetType(Message) Then
            Return pulldata.TryCast(Of Message)
        End If

        Dim g As New NetworkGraph

        For Each link As RegulationFootprint In pulldata.TryCast(Of IEnumerable(Of RegulationFootprint))
            If link.regulator Is Nothing Then
                Continue For
            End If

            Dim u As String = link.regulator.Split("."c).First
            Dim v As String = link.ORF.Split("."c).First

            If g.GetElementByID(u) Is Nothing Then
                Call g.CreateNode(u, New NodeData With {
                    .label = u,
                    .origID = u,
                    .Properties = New Dictionary(Of String, String) From {
                        {"group", link.regulator_group},
                        {"source", link.regulator_trace}
                    }
                })
            End If
            If g.GetElementByID(v) Is Nothing Then
                Call g.CreateNode(v, New NodeData With {
                    .label = v,
                    .origID = v,
                    .Properties = New Dictionary(Of String, String) From {
                        {"group", link.target_group},
                        {"source", link.motif_trace}
                    }
                })
            End If

            Call g.CreateEdge(g.GetElementByID(u), g.GetElementByID(v), 1, New EdgeData With {
                .Properties = New Dictionary(Of String, String) From {
                    {"motif", link.signature},
                    {"site", link.sequence},
                    {"loci", link.starts},
                    {"family", link.motif_family}
                }
            })
        Next

        Return g
    End Function

    <ExportAPI("assign_tffamily")>
    Public Function makeTFFamilyTerms(<RRawVectorArgument> blastp As Object,
                                      Optional TFdb As TFInfo() = Nothing,
                                      Optional env As Environment = Nothing) As Object

        Dim regs As pipeline = pipeline.TryCreatePipeline(Of IQueryHits)(blastp, env)

        If regs.isError Then
            Return regs.getError
        ElseIf TFdb.IsNullOrEmpty Then
            TFdb = env.globalEnvironment _
                .GetResourceFile("data/PlantTFDB/TF.csv", package:="Plantea") _
                .LoadCsv(Of TFInfo)(mute:=True) _
                .ToArray
        End If

        Dim termMaps As New Dictionary(Of String, String)

        For Each tf As TFInfo In TFdb
            termMaps(tf.protein_id) = tf.family
        Next

        Dim pull As IEnumerable(Of IQueryHits) = regs.populates(Of IQueryHits)(env)
        Dim termsAll As RankTerm() = RankTerm.RankTopTerm(pull, termMaps, topBest:=False).ToArray

        Return termsAll
    End Function

    ''' <summary>
    ''' build transcription regulation network
    ''' </summary>
    ''' <param name="motifLinks"></param>
    ''' <param name="motif_hits"></param>
    ''' <param name="regulators">
    ''' should be a blast alignment result of the class type <see cref="RankTerm"/>. apply for mapping protein to a specific family term
    ''' </param>
    ''' <param name="env"></param>
    ''' <returns></returns>
    <ExportAPI("tf_network")>
    <RApiReturn(GetType(RegulationFootprint))>
    Public Function LinkTFNetwork(motifLinks As MotifLink(), motif_hits As MotifMatch(), <RRawVectorArgument> regulators As RankTerm(),
                                  Optional topic As RankTerm() = Nothing,
                                  Optional top As Integer = 3,
                                  Optional env As Environment = Nothing) As Object

        Dim TFdb = env.globalEnvironment _
            .GetResourceFile("data/PlantTFDB/TF.csv", package:="Plantea") _
            .LoadCsv(Of TFInfo)(mute:=True) _
            .ToArray
        Dim regIndex As Dictionary(Of String, RankTerm()) = regulators _
            .Select(Function(a) a.source.Select(Function(p) (p, a))) _
            .IteratesALL _
            .GroupBy(Function(a) a.p) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.Select(Function(t) t.a).ToArray
                          End Function)
        Dim regfamily As Dictionary(Of String, RankTerm()) = regulators _
            .GroupBy(Function(a) a.term) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.ToArray
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
        Dim network As New List(Of RegulationFootprint)
        Dim topics As New Dictionary(Of String, RankTerm)

        If Not topic.IsNullOrEmpty Then
            topics = topic.ToDictionary(Function(a) a.queryName)
        End If

        For Each scan As MotifMatch In TqdmWrapper.Wrap(motif_hits)
            Dim motif_seed = scan.seeds(0).Split.Skip(1).ToArray
            Dim links = matrixIndex(motif_seed.First)
            Dim gene_ids As String() = links.Select(Function(l) l.Gene_id).IteratesALL.ToArray
            Dim regList As New List(Of RankTerm)
            Dim infertype As String = "missing"
            Dim target_meta As String() = scan.title.Split("|"c)

            For Each source_id As String In gene_ids
                ' translate gene_id to TF id
                Dim tf As TFInfo() = TFGeneIndex(source_id)

                If tf.Any(Function(a) regIndex.ContainsKey(a.protein_id)) Then
                    infertype = "direct_mapping"
                    regList.AddRange(tf.Where(Function(a) regIndex.ContainsKey(a.protein_id)).Select(Function(a) regIndex(a.protein_id)).IteratesALL)
                ElseIf tf.Any(Function(a) regfamily.ContainsKey(a.family)) Then
                    ' infer by family
                    infertype = "family propagate"
                    regList.AddRange(tf.Where(Function(a) regfamily.ContainsKey(a.family)).Select(Function(a) regfamily(a.family)).IteratesALL)
                Else
                    ' missing
                End If
            Next

            regList = New List(Of RankTerm)(From t In regList Order By t.score Descending)

            If regList.Any(Function(a) topics.ContainsKey(a.queryName)) Then
                regList = New List(Of RankTerm)(From t As RankTerm
                                                In regList
                                                Where topics.ContainsKey(t.queryName)
                                                Let term_score = topics(t.queryName)
                                                Order By t.score * term_score.score Descending
                                                Select t
                                                Take top)
            Else
                regList = New List(Of RankTerm)(regList.Take(top))
            End If

            For Each regTerm As RankTerm In regList

            Next

            Dim reg_desc = regList.Select(Function(r) r.source).IteratesALL.Distinct.ToArray

            Call network.Add(New RegulationFootprint With {
                .chromosome = target_meta(0),
                .sequence = scan.segment,
                .motif_id = motif_seed.First,
                .signature = scan.motif,
                .tag = scan.seeds(0),
                .regulator_trace = regList.Select(Function(a) a.source).IteratesALL.Distinct.JoinBy("; "),
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
