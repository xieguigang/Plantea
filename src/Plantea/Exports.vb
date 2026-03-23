Imports Microsoft.VisualBasic.CommandLine.Reflection
Imports Microsoft.VisualBasic.Data.Framework
Imports Microsoft.VisualBasic.Data.Framework.IO
Imports Microsoft.VisualBasic.Data.visualize.Network.Graph
Imports Microsoft.VisualBasic.Language
Imports Microsoft.VisualBasic.Linq
Imports Microsoft.VisualBasic.Scripting.MetaData
Imports Microsoft.VisualBasic.Serialization.JSON
Imports Microsoft.VisualBasic.Text.Xml.Models
Imports SMRUCC.genomics.Analysis.HTS.GSEA
Imports SMRUCC.genomics.Analysis.SequenceTools.SequencePatterns.Motif
Imports SMRUCC.genomics.ComponentModel.Annotation
Imports SMRUCC.genomics.Interops.NCBI.Extensions.Pipeline
Imports SMRUCC.genomics.SequenceModel.FASTA
Imports SMRUCC.Rsharp.Runtime
Imports SMRUCC.Rsharp.Runtime.Components
Imports SMRUCC.Rsharp.Runtime.Internal.Object
Imports SMRUCC.Rsharp.Runtime.Interop
Imports SMRUCC.Rsharp.Runtime.Vectorization
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
    ''' create clr model for link the reference TF and the reference motif site model
    ''' </summary>
    ''' <param name="matrix_id">the motif id</param>
    ''' <param name="tf_id">the transcript factor id</param>
    ''' <param name="env"></param>
    ''' <returns></returns>
    <ExportAPI("as.motif_links")>
    Public Function motif_links(<RRawVectorArgument(TypeCodes.string)> matrix_id As Object,
                                <RRawVectorArgument(TypeCodes.string)> tf_id As Object,
                                Optional env As Environment = Nothing) As Object

        Dim matrix_vec = GetVectorElement.Create(Of String)(matrix_id)
        Dim tf_vec = GetVectorElement.Create(Of String)(tf_id)

        If Not GetVectorElement.DoesSizeMatch(matrix_vec, tf_vec) Then
            Return RInternal.debug.stop($"the dimension size of matrix_id({matrix_vec.size}) is mis-matched with the tf_id({tf_vec.size}) vector.", env)
        End If

        Return GetVectorElement.Zip(matrix_vec, tf_vec) _
            .Select(Function(reg)
                        Return New MotifLink With {
                            .Matrix_id = CStr(reg.Item1),
                            .Gene_id = New String() {CStr(reg.Item2)}
                        }
                    End Function) _
            .ToArray
    End Function

    ''' <summary>
    ''' load motif database from a given xml list dataset
    ''' </summary>
    ''' <param name="file">
    ''' the filepath to the xml database file
    ''' </param>
    ''' <returns></returns>
    <ExportAPI("load_motifdb")>
    Public Function loadMotifDb(file As String) As MotifPWM()
        Return file.LoadXml(Of XmlList(Of MotifPWM))() _
            .AsEnumerable _
            .ToArray
    End Function

    ''' <summary>
    ''' read the json list of csv table of the gene cluster data information
    ''' </summary>
    ''' <param name="x"></param>
    ''' <returns></returns>
    <ExportAPI("load_class")>
    Public Function loadClusterBackground(x As String) As ClassClusterData()
        If x.ExtensionSuffix("csv") Then
            Return x.LoadCsv(Of ClassClusterData)(mute:=True)
        Else
            ' read jsonl
            Return x.LineIterators _
                .JoinBy(vbCrLf) _
                .LoadJSON(Of ClassClusterData())
        End If
    End Function

    ''' <summary>
    ''' convert the gene cluster information as gsea background model
    ''' </summary>
    ''' <param name="geneset"></param>
    ''' <param name="env"></param>
    ''' <returns></returns>
    <ExportAPI("class_background")>
    <RApiReturn(GetType(Background))>
    Public Function class_background(<RRawVectorArgument> geneset As Object, Optional env As Environment = Nothing) As Object
        Dim genes As pipeline = pipeline.TryCreatePipeline(Of ClassClusterData)(geneset, env)

        If genes.isError Then
            Return genes.getError
        Else
            Return ClassClusterData.BuildBackground(genes.populates(Of ClassClusterData)(env))
        End If
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

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="blastp">diamond annotation hit table</param>
    ''' <param name="TFdb"></param>
    ''' <param name="top_best"></param>
    ''' <param name="env"></param>
    ''' <returns></returns>
    <ExportAPI("assign_tffamily")>
    <RApiReturn(GetType(RankTerm))>
    Public Function makeTFFamilyTerms(<RRawVectorArgument> blastp As Object,
                                      Optional TFdb As TFInfo() = Nothing,
                                      Optional top_best As Boolean = True,
                                      Optional identities As Double = 30,
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

        Dim pull As IEnumerable(Of IQueryHits) = From p As IQueryHits
                                                 In regs.populates(Of IQueryHits)(env)
                                                 Where p.identities > identities
        Dim termsAll As RankTerm() = RankTerm.RankTopTerm(pull, termMaps, topBest:=top_best).ToArray

        Return termsAll
    End Function
End Module
