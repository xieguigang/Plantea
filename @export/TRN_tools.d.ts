// export R# package module type define for javascript/typescript language
//
//    imports "TRN_tools" from "Plantea";
//
// ref=Plantea.TRNTools@Plantea, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null

/**
 * 
*/
declare namespace TRN_tools {
   /**
    * assign the class information to the ORF inside TRN network
    * 
    * 
     * @param regs the TRN network data
     * @param kb -
     * @param env -
     * 
     * + default value Is ``null``.
   */
   function assign_classdata(regs: any, kb: object, env?: object): object;
   /**
     * @param env default value Is ``null``.
   */
   function bbh_mapping(regs: any, bbh: any, env?: object): any;
   /**
    * read regulation network from a given csv table file
    * 
    * 
     * @param file -
     * @param tqdm 
     * + default value Is ``true``.
   */
   function read_regulation(file: string, tqdm?: boolean): object;
   /**
    * create subnetwork by matches a set of terms
    * 
    * 
     * @param regulations -
     * @param terms -
     * @param env -
     * 
     * + default value Is ``null``.
   */
   function term_subnetwork(regulations: any, terms: any, env?: object): object;
   /**
    * build transcription regulation network
    * 
    * 
     * @param motifLinks -
     * @param motif_hits -
     * @param regulators should be a blast alignment result of the class type @``T:SMRUCC.genomics.Interops.NCBI.Extensions.Pipeline.RankTerm``. apply for mapping protein to a specific family term
     * @param tfinfo 
     * + default value Is ``null``.
     * @param topic 
     * + default value Is ``null``.
     * @param top take the top n tf regulator mapping result for build TRN network.
     * 
     * + default value Is ``3``.
     * @param env -
     * 
     * + default value Is ``null``.
   */
   function tf_network(motifLinks: object, motif_hits: any, regulators: object, tfinfo?: object, topic?: object, top?: object, env?: object): object;
}
