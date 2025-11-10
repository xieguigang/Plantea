Imports Microsoft.VisualBasic.ApplicationServices.Terminal.ProgressBar.Tqdm
Imports Microsoft.VisualBasic.Linq
Imports SMRUCC.genomics.Analysis.SequenceTools.SequencePatterns.Motif
Imports SMRUCC.genomics.Interops.NCBI.Extensions.Pipeline

Public Class RegulationNetwork

    ReadOnly TFGeneIndex As Dictionary(Of String, TFInfo())
    ReadOnly matrixIndex As Dictionary(Of String, MotifLink())

    Sub New(motifLinks As MotifLink(), TFdb As TFInfo())
        TFGeneIndex = TFdb _
            .GroupBy(Function(a) a.gene_id) _
            .OrderByDescending(Function(a) a.Count) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.ToArray
                          End Function)
        matrixIndex = motifLinks _
            .GroupBy(Function(a) a.Matrix_id) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.ToArray
                          End Function)
    End Sub

    Dim regIndex As Dictionary(Of String, RankTerm())
    Dim regfamily As Dictionary(Of String, RankTerm())
    Dim topics As Dictionary(Of String, RankTerm)

    Public Iterator Function BuildTFNetwork(motif_hits As MotifMatch(), regulators As RankTerm(),
                                            Optional topic As RankTerm() = Nothing,
                                            Optional top As Integer = 3) As IEnumerable(Of RegulationFootprint)
        regIndex = regulators _
            .Select(Function(a) a.source.Select(Function(p) (p, a))) _
            .IteratesALL _
            .GroupBy(Function(a) a.p) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.Select(Function(t) t.a).ToArray
                          End Function)
        regfamily = regulators _
            .GroupBy(Function(a) a.term) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.ToArray
                          End Function)
        topics = New Dictionary(Of String, RankTerm)

        If Not topic.IsNullOrEmpty Then
            topics = topic.ToDictionary(Function(a) a.queryName)
        End If

        For Each scan As MotifMatch In TqdmWrapper.Wrap(motif_hits)
            For Each edge As RegulationFootprint In LinkTFNetwork(scan, top)
                Yield edge
            Next
        Next
    End Function

    Private Iterator Function LinkTFNetwork(scan As MotifMatch, top As Integer) As IEnumerable(Of RegulationFootprint)
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
            Dim reg_desc As String = regTerm.topHit
            Dim edge As New RegulationFootprint With {
                .chromosome = target_meta(0),
                .sequence = scan.segment,
                .motif_id = motif_seed.First,
                .signature = scan.motif,
                .tag = scan.seeds(0),
                .regulator_trace = $"{reg_desc},score={regTerm.scores.Max}",
                .regulator = regTerm.queryName,
                .ORF = target_meta(1),
                .motif_family = reg_desc,
                .motif_trace = scan.seeds.First,
                .distance = -scan.start,
                .pvalue = scan.pvalue,
                .type = infertype
            }

            Yield edge
        Next
    End Function
End Class
