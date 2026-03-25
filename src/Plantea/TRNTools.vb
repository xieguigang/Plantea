Imports Microsoft.VisualBasic.CommandLine.Reflection
Imports Microsoft.VisualBasic.Data.Framework
Imports Microsoft.VisualBasic.Data.Framework.IO.Linq
Imports Microsoft.VisualBasic.Linq
Imports Microsoft.VisualBasic.Scripting.MetaData
Imports PlantToolKit
Imports SMRUCC.genomics.Analysis.SequenceTools.SequencePatterns
Imports SMRUCC.genomics.Interops.NCBI.Extensions.LocalBLAST.Application.BBH
Imports SMRUCC.genomics.Interops.NCBI.Extensions.Pipeline
Imports SMRUCC.genomics.SequenceModel.FASTA
Imports SMRUCC.Rsharp.Runtime
Imports SMRUCC.Rsharp.Runtime.Components
Imports SMRUCC.Rsharp.Runtime.Internal.Object
Imports SMRUCC.Rsharp.Runtime.Interop
Imports SMRUCC.Rsharp.Runtime.Vectorization

<Package("TRN_tools")>
Module TRNTools

    ''' <summary>
    ''' build transcription regulation network
    ''' </summary>
    ''' <param name="motifLinks"></param>
    ''' <param name="motif_hits"></param>
    ''' <param name="regulators">
    ''' should be a blast alignment result of the class type <see cref="RankTerm"/>. apply for mapping protein to a specific family term
    ''' </param>
    ''' <param name="top">
    ''' take the top n tf regulator mapping result for build TRN network.
    ''' </param>
    ''' <param name="env"></param>
    ''' <returns></returns>
    <ExportAPI("tf_network")>
    <RApiReturn(GetType(RegulationFootprint))>
    Public Function LinkTFNetwork(motifLinks As MotifLink(), <RRawVectorArgument> motif_hits As Object, <RRawVectorArgument> regulators As RankTerm(),
                                  Optional tfinfo As TFInfo() = Nothing,
                                  Optional topic As RankTerm() = Nothing,
                                  Optional top As Integer = 3,
                                  Optional env As Environment = Nothing) As Object

        Dim pull = pipeline.TryCreatePipeline(Of MotifMatch)(motif_hits, env)

        If pull.isError Then
            Return pull.getError
        End If

        Dim TFdb As TFInfo() = If(tfinfo, env.globalEnvironment _
            .GetResourceFile("data/PlantTFDB/TF.csv", package:="Plantea") _
            .LoadCsv(Of TFInfo)(mute:=True) _
            .ToArray)
        Dim sites As MotifMatch() = pull.populates(Of MotifMatch)(env).ToArray
        Dim network As New RegulationNetwork(motifLinks, TFdb)
        Dim regs As RegulationFootprint() = network _
            .BuildTFNetwork(sites, regulators, topic, top) _
            .ToArray

        Return regs
    End Function

    ''' <summary>
    ''' read regulation network from a given csv table file
    ''' </summary>
    ''' <param name="file"></param>
    ''' <returns></returns>
    <ExportAPI("read_regulation")>
    <RApiReturn(GetType(RegulationFootprint))>
    Public Function readRegulations(file As String, Optional tqdm As Boolean = True) As Object
        If tqdm Then
            Return file.OpenHandle.AsLinq(Of RegulationFootprint).as_iterator
        Else
            Return file.LoadCsv(Of RegulationFootprint)(mute:=True).ToArray
        End If
    End Function

    <ExportAPI("bbh_mapping")>
    Public Function bbh_mapping(<RRawVectorArgument> regs As Object, <RRawVectorArgument> bbh As Object, Optional env As Environment = Nothing) As Object
        Dim pulldata = pullNetwork(regs, env)
        Dim pullbbh As pipeline = pipeline.TryCreatePipeline(Of BiDirectionalBesthit)(bbh, env)

        If pulldata Like GetType(Message) Then
            Return pulldata.TryCast(Of Message)
        ElseIf pullbbh.isError Then
            Return pullbbh.getError
        End If

        Dim bbhIndex = pullbbh.populates(Of BiDirectionalBesthit)(env) _
            .Where(Function(map) map.level <> Levels.NA) _
            .GroupBy(Function(a) a.QueryName) _
            .ToDictionary(Function(a) a.Key,
                          Function(a)
                              Return a.Select(Function(ai) ai.HitName) _
                                  .Distinct _
                                  .ToArray
                          End Function)

        Return pipeline.CreateFromPopulator(MapBBH(pulldata.TryCast(Of IEnumerable(Of RegulationFootprint)), bbhIndex))
    End Function

    Private Iterator Function MapBBH(trn As IEnumerable(Of RegulationFootprint), bbhmap As Dictionary(Of String, String())) As IEnumerable(Of RegulationFootprint)
        For Each edge As RegulationFootprint In trn
            If edge.ORF Is Nothing OrElse edge.regulator Is Nothing Then
                Continue For
            Else
                edge.regulator = HeaderFormats.TrimAccessionVersion(edge.regulator)
            End If
            If (Not bbhmap.ContainsKey(edge.ORF)) OrElse (Not bbhmap.ContainsKey(edge.regulator)) Then
                Continue For
            End If

            Dim orf_ids As String() = bbhmap(edge.ORF)
            Dim reg_ids As String() = bbhmap(edge.regulator)

            For Each orf As String In orf_ids
                For Each reg As String In reg_ids
                    Dim copy As New RegulationFootprint(edge)
                    copy.target_group = orf
                    copy.regulator_group = reg
                    Yield copy
                Next
            Next
        Next
    End Function

    ''' <summary>
    ''' assign the class information to the ORF inside TRN network
    ''' </summary>
    ''' <param name="regs">the TRN network data</param>
    ''' <param name="kb"></param>
    ''' <param name="env"></param>
    ''' <returns></returns>
    <ExportAPI("assign_classdata")>
    <RApiReturn(GetType(RegulationFootprint))>
    Public Function assign_classdata(<RRawVectorArgument> regs As Object, kb As ClassClusterData(), Optional env As Environment = Nothing) As Object
        Dim pulldata = pullNetwork(regs, env)

        If pulldata Like GetType(Message) Then
            Return pulldata.TryCast(Of Message)
        End If

        Dim kbIndex = kb.GroupBy(Function(a) a.gene).ToDictionary(Function(a) a.Key, Function(a) a.First)
        Dim filled As RegulationFootprint() = RegulationFootprint _
            .AssignClassData(pulldata.TryCast(Of IEnumerable(Of RegulationFootprint)), kbIndex) _
            .Where(Function(a) Not a.class.StringEmpty(, True)) _
            .ToArray

        Return filled
    End Function

    ''' <summary>
    ''' create subnetwork by matches a set of terms
    ''' </summary>
    ''' <param name="regulations"></param>
    ''' <param name="terms">bbh hit map data</param>
    ''' <param name="env"></param>
    ''' <returns></returns>
    <ExportAPI("term_subnetwork")>
    <RApiReturn(GetType(RegulationFootprint))>
    Public Function subnetwork(<RRawVectorArgument> regulations As Object, <RRawVectorArgument> terms As Object, Optional env As Environment = Nothing) As Object
        Dim pulldata = pullNetwork(regulations, env)
        Dim rankTerms As pipeline = pipeline.TryCreatePipeline(Of RankTerm)(terms, env)
        Dim idIndex As Boolean = False

        If pulldata Like GetType(Message) Then
            Return pulldata.TryCast(Of Message)
        ElseIf rankTerms.isError Then
            ' check is string?
            Dim idset = CLRVector.asCharacter(terms)
            Dim idterms = idset.Distinct.Select(Function(id) RankTerm.WrapID(id)).ToArray

            ' the source input is id set
            ' no needs for make updates of the 
            ' target group and regulator group for indicates the mapping source id
            idIndex = True
            rankTerms = pipeline.CreateFromPopulator(idterms)
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

                ' needs make records of the mapping source id
                If Not idIndex Then
                    link.target_group = termsIndex(link.ORF).term
                End If
            End If
            If link.regulator IsNot Nothing AndAlso termsIndex.ContainsKey(link.regulator) Then
                hit = True

                ' needs make records of the mapping source id
                If Not idIndex Then
                    link.regulator_group = termsIndex(link.regulator).term
                End If
            End If

            If hit Then
                Call subnet.Add(link)
            End If
        Next

        Return subnet.ToArray
    End Function
End Module
