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
        Dim tfdata As New List(Of TFInfo)

        For Each source_id As String In gene_ids
            If Not TFGeneIndex.ContainsKey(source_id) Then
                Continue For
            End If

            ' translate gene_id to TF id
            Dim tf As TFInfo() = TFGeneIndex(source_id)

            If tf.Any(Function(a) regIndex.ContainsKey(a.protein_id)) Then
                Dim mapped As IEnumerable(Of RankTerm) = tf _
                    .Where(Function(a) regIndex.ContainsKey(a.protein_id)) _
                    .Select(Function(a) regIndex(a.protein_id)) _
                    .IteratesALL

                If infertype = "direct_mapping" Then
                    ' merge with previous source_id result
                    regList.AddRange(mapped)
                Else
                    ' overrides of the family propagate result
                    infertype = "direct_mapping"
                    regList = New List(Of RankTerm)(mapped)
                End If
            ElseIf tf.Any(Function(a) regfamily.ContainsKey(a.family)) Then
                If infertype = "direct_mapping" Then
                    Continue For
                End If

                ' infer by family
                infertype = "family propagate"
                regList.AddRange(tf.Where(Function(a) regfamily.ContainsKey(a.family)).Select(Function(a) regfamily(a.family)).IteratesALL)
            Else
                ' missing
            End If

            Call tfdata.AddRange(tf)
        Next

        Dim motif_family As IGrouping(Of String, TFInfo) = tfdata _
            .GroupBy(Function(a) a.family) _
            .OrderByDescending(Function(a) a.Count) _
            .FirstOrDefault

        If regList.Count = 0 Then
            Dim familyName As String = If(
                motif_family Is Nothing OrElse motif_family.Key Is Nothing,
                "Unknown",
                motif_family.Key)

            ' is missing of the cooresponding regulator
            Yield New RegulationFootprint With {
                .chromosome = target_meta(0),
                .sequence = scan.segment,
                .motif_id = motif_seed.First,
                .signature = scan.motif,
                .tag = Nothing,
                .regulator_trace = Nothing,
                .regulator = Nothing,
                .ORF = target_meta(1),
                .motif_family = familyName,
                .motif_trace = scan.seeds.First,
                .distance = -scan.start,
                .pvalue = scan.pvalue,
                .type = infertype
            }
        Else
            regList = New List(Of RankTerm)(From t As RankTerm
                                            In regList
                                            Order By t.score Descending)

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
                Dim edge As New RegulationFootprint With {
                    .chromosome = target_meta(0),
                    .sequence = scan.segment,
                    .motif_id = motif_seed.First,
                    .signature = scan.motif,
                    .tag = $"{regTerm.topHit},score={regTerm.scores.Max}",
                    .regulator_trace = regTerm.topHit,
                    .regulator = regTerm.queryName,
                    .ORF = target_meta(1),
                    .motif_family = motif_family.Key,
                    .motif_trace = scan.seeds.First,
                    .distance = -scan.start,
                    .pvalue = scan.pvalue,
                    .type = infertype
                }

                Yield edge
            Next
        End If
    End Function
End Class
