// export R# package module type define for javascript/typescript language
//
//    imports "Plantea" from "Plantea";
//
// ref=Plantea.Exports@Plantea, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null

/**
 * 
*/
declare namespace Plantea {
   /**
    * load motif database from a given xml list dataset
    * 
    * 
   */
   function load_motifdb(file: string): object;
   /**
    * 
    * 
     * @param motifLinks -
     * @param motif_hits -
     * @param regulators should be a blast alignment result of the subclass of @``T:SMRUCC.genomics.Interops.NCBI.Extensions.LocalBLAST.Application.BBH.Abstract.IQueryHits``.
     * @param env -
     * 
     * + default value Is ``null``.
   */
   function tf_network(motifLinks: object, motif_hits: object, regulators: any, env?: object): object;
}
