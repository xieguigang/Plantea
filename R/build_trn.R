#' buildd TRN network
const build_trn = function(sites, diamond, link_code) {
    let tf_family = motif_tfinfo(link_code);
    let tf = assign_tffamily(diamond, TFdb = tf_family, identities = 60);
    let reg_links = load_motif_links(code = link_code, data_frame = FALSE);
 
}