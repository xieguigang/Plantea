// export R# source type define for javascript/typescript language
//
// package_source=Plantea

declare namespace Plantea {
   module _ {
      /**
      */
      function onLoad(): object;
   }
   /**
   */
   function build_trn(sites: any, diamond: any, link_code: any): object;
   /**
     * @param data_frame default value Is ``false``.
   */
   function load_global_links(data_frame?: any): object;
   /**
     * @param code default value Is ``null``.
     * @param data_frame default value Is ``false``.
   */
   function load_motif_links(code?: any, data_frame?: any): object;
   /**
     * @param data_frame default value Is ``false``.
   */
   function load_taxonomy_links(code: any, data_frame?: any): object;
   /**
   */
   function locate_meme_dir(code: any): object;
   /**
   */
   function motif_tfinfo(code: any): object;
   /**
   */
   function motif_tfseq(code: any): object;
   /**
   */
   function open_meme(code: any): object;
   /**
     * @param outdir default value Is ``./``.
     * @param n_threads default value Is ``8``.
   */
   function plant_motif_search(seqfile: any, plant_code: any, outdir?: any, n_threads?: any): object;
   /**
   */
   function PlantTFDB_motifs(): object;
}
