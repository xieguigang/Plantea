// export R# package module type define for javascript/typescript language
//
//    imports "Plantea" from "Plantea";
//
// ref=Plantea.Exports@Plantea, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null

/**
 * The plant genomics data analysis tools
 * 
*/
declare namespace Plantea {
   module as {
      /**
       * create clr model for link the reference TF and the reference motif site model
       * 
       * 
        * @param matrix_id the motif id
        * @param tf_id the transcript factor id
        * @param env -
        * 
        * + default value Is ``null``.
      */
      function motif_links(matrix_id: any, tf_id: any, env?: object): any;
      /**
        * @param env default value Is ``null``.
      */
      function regulation_graph(regulations: any, env?: object): object;
   }
   /**
     * @param env default value Is ``null``.
   */
   function assign_classdata(regs: any, kb: object, env?: object): object;
   /**
    * 
    * 
     * @param blastp diamond annotation hit table
     * @param TFdb -
     * 
     * + default value Is ``null``.
     * @param top_best -
     * 
     * + default value Is ``true``.
     * @param identities 
     * + default value Is ``30``.
     * @param env -
     * 
     * + default value Is ``null``.
   */
   function assign_tffamily(blastp: any, TFdb?: object, top_best?: boolean, identities?: number, env?: object): object;
   /**
    * convert the gene cluster information as gsea background model
    * 
    * 
     * @param geneset -
     * @param env -
     * 
     * + default value Is ``null``.
   */
   function class_background(geneset: any, env?: object): object;
   /**
     * @param env default value Is ``null``.
   */
   function count_matrix(regulations: any, env?: object): any;
   /**
    * A helper function extract fo the PlantTFDB information
    * 
    * 
     * @param TF_fsa -
     * @param env -
     * 
     * + default value Is ``null``.
   */
   function extract_tf_info(TF_fsa: any, env?: object): object;
   /**
    * read the json list of csv table of the gene cluster data information
    * 
    * 
     * @param x -
   */
   function load_class(x: string): object;
   /**
    * load motif database from a given xml list dataset
    * 
    * 
     * @param file the filepath to the xml database file
   */
   function load_motifdb(file: string): object;
   /**
    * read regulation network from a given csv table file
    * 
    * 
     * @param file -
   */
   function read_regulation(file: string): object;
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
