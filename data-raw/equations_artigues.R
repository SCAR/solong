alleq_arti <- function(id) {
    switch(id,
           "234696_mass~TL_Arti2003"=list(taxon_name="Dolloidraco longedorsali",
                                          taxon_aphia_id=234696,
                                          equation=function(...){a=0.00725; b=3.013; se=0.184;
                                              tibble(allometric_value=a*(...^b),
                                                     allometric_value_lower=10^(log10(a)+b*log10(...)-1.96*se),
                                                     allometric_value_upper=10^(log10(a)+b*log10(...)+1.96*se))
                                              },
                                          inputs=tibble(property="total length",units="cm"),
                                          return_property="mass",
                                          return_units="g",
                                          reliability=tribble(~type,~value,
                                                              "N",371,
                                                              "R^2",0.918^2),
                                          reference=refs$Arti2003),

           stop("unrecognized equation ID: ",id))
}
