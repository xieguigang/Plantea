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
    * load motif database from a given xml list dataset
    * 
    * 
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
    * build transcription regulation network
    * 
    * 
     * @param motifLinks -
     * @param motif_hits -
     * @param regulators should be a blast alignment result of the subclass of @``T:SMRUCC.genomics.ComponentModel.Annotation.IQueryHits``.
     * @param env -
     * 
     * + default value Is ``null``.
   */
   function tf_network(motifLinks: object, motif_hits: object, regulators: any, env?: object): object;
}
