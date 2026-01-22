gtfs_agency_canonical_map <- tribble(
  ~agency_name                                             , ~canonical_agency_id            , ~canonical_agency_name                    ,

  # ========== MONTREAL REGION (QC) ==========

  # exo / RTM / AMT - Main regional network
  "Agence métropolitaine de transport"                    , "CA_QC_MTL_EXO"                 , "exo"                                     ,
  "Agence métropolitaine de transport (Express)"          , "CA_QC_MTL_EXO"                 , "exo"                                     ,
  "Réseau de transport métropolitain"                    , "CA_QC_MTL_EXO"                 , "exo"                                     ,
  "exo-Réseau de transport métropolitain"                , "CA_QC_MTL_EXO"                 , "exo"                                     ,
  "exo - Réseau de transport métropolitain"              , "CA_QC_MTL_EXO"                 , "exo"                                     ,

  # exo - Chambly-Richelieu-Carignan
  "exo-Chambly-Richelieu-Carignan"                         , "CA_QC_MTL_EXO_CHAMBLY"         , "exo-Chambly-Richelieu-Carignan"          ,
  "exo-Chambly - Richelieu - Carignan"                     , "CA_QC_MTL_EXO_CHAMBLY"         , "exo-Chambly-Richelieu-Carignan"          ,
  "exo-Chambly - Roussillon - Carignan"                    , "CA_QC_MTL_EXO_CHAMBLY"         , "exo-Chambly-Richelieu-Carignan"          ,
  "RTM Chambly-Richelieu-Carignan"                         , "CA_QC_MTL_EXO_CHAMBLY"         , "exo-Chambly-Richelieu-Carignan"          ,
  "CIT Chambly-Richelieu-Carignan"                         , "CA_QC_MTL_EXO_CHAMBLY"         , "exo-Chambly-Richelieu-Carignan"          ,

  # exo - Laurentides
  "exo-Laurentides"                                        , "CA_QC_MTL_EXO_LAURENTIDES"     , "exo-Laurentides"                         ,
  "RTM Laurentides"                                        , "CA_QC_MTL_EXO_LAURENTIDES"     , "exo-Laurentides"                         ,
  "CIT Laurentides"                                        , "CA_QC_MTL_EXO_LAURENTIDES"     , "exo-Laurentides"                         ,
  "CIT des Laurentides"                                    , "CA_QC_MTL_EXO_LAURENTIDES"     , "exo-Laurentides"                         ,

  # exo - Haut-Saint-Laurent
  "exo-Haut-Saint-Laurent"                                 , "CA_QC_MTL_EXO_HSL"             , "exo-Haut-Saint-Laurent"                  ,
  "RTM Haut-Saint-Laurent"                                 , "CA_QC_MTL_EXO_HSL"             , "exo-Haut-Saint-Laurent"                  ,
  "CIT du Haut-Saint-Laurent"                              , "CA_QC_MTL_EXO_HSL"             , "exo-Haut-Saint-Laurent"                  ,

  # exo - La Presqu'île
  "exo-La Presqu'île"                                     , "CA_QC_MTL_EXO_PRESQUILE"       , "exo-La Presqu'île"                      ,
  "RTM La Presqu'île"                                     , "CA_QC_MTL_EXO_PRESQUILE"       , "exo-La Presqu'île"                      ,
  "CIT La Presqu'île"                                     , "CA_QC_MTL_EXO_PRESQUILE"       , "exo-La Presqu'île"                      ,

  # exo - Le Richelain
  "exo-Le Richelain"                                       , "CA_QC_MTL_EXO_RICHELAIN"       , "exo-Le Richelain"                        ,
  "exo-Le Richelain / Roussillon"                          , "CA_QC_MTL_EXO_RICHELAIN"       , "exo-Le Richelain"                        ,
  "RTM Le Richelain"                                       , "CA_QC_MTL_EXO_RICHELAIN"       , "exo-Le Richelain"                        ,
  "CIT Le Richelain"                                       , "CA_QC_MTL_EXO_RICHELAIN"       , "exo-Le Richelain"                        ,

  # exo - Roussillon
  "exo-Roussillon"                                         , "CA_QC_MTL_EXO_ROUSSILLON"      , "exo-Roussillon"                          ,
  "RTM Roussillon"                                         , "CA_QC_MTL_EXO_ROUSSILLON"      , "exo-Roussillon"                          ,
  "CIT Roussillon"                                         , "CA_QC_MTL_EXO_ROUSSILLON"      , "exo-Roussillon"                          ,

  # exo - Sorel-Varennes
  "exo-Sorel-Varennes"                                     , "CA_QC_MTL_EXO_SOREL"           , "exo-Sorel-Varennes"                      ,
  "RTM Sorel-Varennes"                                     , "CA_QC_MTL_EXO_SOREL"           , "exo-Sorel-Varennes"                      ,
  "CIT Sorel-Varennes"                                     , "CA_QC_MTL_EXO_SOREL"           , "exo-Sorel-Varennes"                      ,

  # exo - Sud-Ouest
  "exo-Sud-Ouest"                                          , "CA_QC_MTL_EXO_SUDOUEST"        , "exo-Sud-Ouest"                           ,
  "RTM Sud-ouest"                                          , "CA_QC_MTL_EXO_SUDOUEST"        , "exo-Sud-Ouest"                           ,
  "CIT Sud-ouest"                                          , "CA_QC_MTL_EXO_SUDOUEST"        , "exo-Sud-Ouest"                           ,

  # exo - Vallée du Richelieu
  "exo-Vallée du Richelieu"                               , "CA_QC_MTL_EXO_VALLEE"          , "exo-Vallée du Richelieu"                ,
  "RTM Vallée du Richelieu"                               , "CA_QC_MTL_EXO_VALLEE"          , "exo-Vallée du Richelieu"                ,
  "RTM Vallée-du-Richelieu"                               , "CA_QC_MTL_EXO_VALLEE"          , "exo-Vallée du Richelieu"                ,
  "CIT Vallée-du-Richelieu"                               , "CA_QC_MTL_EXO_VALLEE"          , "exo-Vallée du Richelieu"                ,

  # exo - L'Assomption
  "exo-L'Assomption"                                       , "CA_QC_MTL_EXO_ASSOMPTION"      , "exo-L'Assomption"                        ,
  "exo-L'ASSOMPTION"                                       , "CA_QC_MTL_EXO_ASSOMPTION"      , "exo-L'Assomption"                        ,
  "RTM L'Assomption"                                       , "CA_QC_MTL_EXO_ASSOMPTION"      , "exo-L'Assomption"                        ,
  "MRC de L'Assomption"                                    , "CA_QC_MTL_EXO_ASSOMPTION"      , "exo-L'Assomption"                        ,
  "CRT Lanaudière"                                        , "CA_QC_MTL_EXO_ASSOMPTION"      , "exo-L'Assomption"                        ,

  # exo - Terrebonne-Mascouche
  "exo-Terrebonne-Mascouche"                               , "CA_QC_MTL_EXO_TERREBONNE"      , "exo-Terrebonne-Mascouche"                ,
  "RTM Terrebonne-Mascouche"                               , "CA_QC_MTL_EXO_TERREBONNE"      , "exo-Terrebonne-Mascouche"                ,
  "MRC Les Moulins (Urbis)"                                , "CA_QC_MTL_EXO_TERREBONNE"      , "exo-Terrebonne-Mascouche"                ,
  "Les Moulins (Urbis)"                                    , "CA_QC_MTL_EXO_TERREBONNE"      , "exo-Terrebonne-Mascouche"                ,
  "MRC les Moulins"                                        , "CA_QC_MTL_EXO_TERREBONNE"      , "exo-Terrebonne-Mascouche"                ,

  # exo - Sainte-Julie
  "exo-Sainte-Julie"                                       , "CA_QC_MTL_EXO_STJULIE"         , "exo-Sainte-Julie"                        ,
  "RTM Sainte-Julie"                                       , "CA_QC_MTL_EXO_STJULIE"         , "exo-Sainte-Julie"                        ,
  "OMIT Sainte-Julie"                                      , "CA_QC_MTL_EXO_STJULIE"         , "exo-Sainte-Julie"                        ,

  # exo - Deux-Montagnes
  "exo - Deux-Montagnes (Express d'Oka)"                   , "CA_QC_MTL_EXO_DEUXMONTAGNES"   , "exo-Deux-Montagnes"                      ,

  # STM - Montreal
  "Société de transport de Montréal"                    , "CA_QC_MTL_STM"                 , "STM"                                     ,
  "Société de Transport de Montréal"                    , "CA_QC_MTL_STM"                 , "STM"                                     ,

  # REM - Montreal automated metro
  "Réseau express métropolitain"                         , "CA_QC_MTL_REM"                 , "REM"                                     ,
  "Réseau Express Métropolitain"                         , "CA_QC_MTL_REM"                 , "REM"                                     ,

  # ========== OTHER QUEBEC ==========

  "Réseau de transport de la Capitale (RTC)"              , "CA_QC_QUEBEC_RTC"              , "RTC Québec"                             ,
  "Réseau de transport de la capitale (RTC)"              , "CA_QC_QUEBEC_RTC"              , "RTC Québec"                             ,
  "Réseau de transport de Longueuil"                      , "CA_QC_LONGUEUIL_RTL"           , "RTL"                                     ,
  "Societe de transport de Laval"                          , "CA_QC_LAVAL_STL"               , "STL"                                     ,
  "Société de transport de Lévis"                       , "CA_QC_LEVIS_STL"               , "STL Lévis"                              ,
  "STO"                                                    , "CA_QC_GATINEAU_STO"            , "STO"                                     ,
  "Société de Transport de Sherbrooke"                   , "CA_QC_SHERBROOKE_STS"          , "STS"                                     ,
  "Service opéré par Taxis de Sherbrooke"                , "CA_QC_SHERBROOKE_STS"          , "STS"                                     ,
  "Société de Transport de Trois-Rivières"              , "CA_QC_TROISRIVIERES_STTR"      , "STTR"                                    ,
  "Transcollines"                                          , "CA_QC_OUTAOUAIS_TRANSCOLLINES" , "Transcollines"                           ,
  "RÉGIM"                                                 , "CA_QC_RIMOUSKI_REGIM"          , "RÉGIM"                                  ,
  "Société des transports de Rimouski"                   , "CA_QC_RIMOUSKI_REGIM"          , "RÉGIM"                                  ,
  "Train de Charlevoix"                                    , "CA_QC_CHARLEVOIX_TRAIN"        , "Train de Charlevoix"                     ,
  "Ville de Drummondville"                                 , "CA_QC_DRUMMONDVILLE_TD"        , "Transit Drummondville"                   ,
  "Saint-Hyacinthe"                                        , "CA_QC_SAINTHYACINTHE_TASH"     , "TASH"                                    ,
  "Ville de Saint-Jean-sur-Richelieu"                      , "CA_QC_SAINTJEAN_TSSJR"         , "Transport Saint-Jean"                    ,
  "Ville de Rouyn-Noranda"                                 , "CA_QC_ROUYN_CITRN"             , "CIT Rouyn-Noranda"                       ,
  "MRC de montcalm"                                        , "CA_QC_MONTCALM_MRC"            , "MRC Montcalm"                            ,
  "Transport en commun de Mont-Tremblant"                  , "CA_QC_MONTTREMBLANT_TCMT"      , "Transport Mont-Tremblant"                ,
  "Express Lotbinière"                                    , "CA_QC_LOTBINIERE_EL"           , "Express Lotbinière"                     ,
  "Orléans Express"                                       , "CA_QC_INTERCITY_ORLEANS"       , "Orléans Express"                        ,
  "Bili"                                                   , "CA_QC_INTERCITY_BILI"          , "Bili"                                    ,
  "RTCS"                                                   , "CA_QC_INTERCITY_RTCS"          , "RTCS"                                    ,

  # ========== BC TRANSIT SYSTEMS ==========

  "BC Transit - Victoria Regional Transit System"          , "CA_BC_VICTORIA_BCT"            , "BC Transit - Victoria"                   ,
  "BC Transit - Victoria Regional Transi"                  , "CA_BC_VICTORIA_BCT"            , "BC Transit - Victoria"                   ,
  "BC Transit - Central Fraser Valley Transit System"      , "CA_BC_CFV_BCT"                 , "BC Transit - Central Fraser Valley"      ,
  "BC Transit - Central Fraser Valley Tr"                  , "CA_BC_CFV_BCT"                 , "BC Transit - Central Fraser Valley"      ,
  "BC Transit - Kelowna Regional Transit System"           , "CA_BC_KELOWNA_BCT"             , "BC Transit - Kelowna"                    ,
  "BC Transit - Kelowna Regional Transit"                  , "CA_BC_KELOWNA_BCT"             , "BC Transit - Kelowna"                    ,
  "BC Transit - Prince George Transit System"              , "CA_BC_PRINCEG_BCT"             , "BC Transit - Prince George"              ,
  "BC Transit - Prince George Transit Sy"                  , "CA_BC_PRINCEG_BCT"             , "BC Transit - Prince George"              ,
  "BC Transit - Comox Valley Transit System"               , "CA_BC_COMOX_BCT"               , "BC Transit - Comox Valley"               ,
  "BC Transit - Comox Valley Transit Sys"                  , "CA_BC_COMOX_BCT"               , "BC Transit - Comox Valley"               ,
  "BC Transit - Sunshine Coast Transit System"             , "CA_BC_SUNSHINE_BCT"            , "BC Transit - Sunshine Coast"             ,
  "BC Transit - Sunshine Coast Transit S"                  , "CA_BC_SUNSHINE_BCT"            , "BC Transit - Sunshine Coast"             ,
  "BC Transit - Chilliwack Transit System"                 , "CA_BC_CHILLIWACK_BCT"          , "BC Transit - Chilliwack"                 ,
  "BC Transit - Chilliwack Transit Syste"                  , "CA_BC_CHILLIWACK_BCT"          , "BC Transit - Chilliwack"                 ,
  "BC Transit - Kamloops Transit System"                   , "CA_BC_KAMLOOPS_BCT"            , "BC Transit - Kamloops"                   ,
  "BC Transit - RDN Transit System"                        , "CA_BC_RDN_BCT"                 , "BC Transit - RDN"                        ,
  "BC Transit - Squamish Transit System"                   , "CA_BC_SQUAMISH_BCT"            , "BC Transit - Squamish"                   ,
  "BC Transit - Whistler Transit System"                   , "CA_BC_WHISTLER_BCT"            , "BC Transit - Whistler"                   ,
  "BC Transit - Fraser Valley Express"                     , "CA_BC_FVX_BCT"                 , "BC Transit - Fraser Valley Express"      ,
  "BC Transit - Campbell River Transit System"             , "CA_BC_CAMPBELLR_BCT"           , "BC Transit - Campbell River"             ,
  "BC Transit - Cowichan Valley Regional Transit System"   , "CA_BC_COWICHAN_BCT"            , "BC Transit - Cowichan Valley"            ,
  "BC Transit - Cranbrook Transit System"                  , "CA_BC_CRANBROOK_BCT"           , "BC Transit - Cranbrook"                  ,
  "BC Transit - Fort St John Transit System"               , "CA_BC_FORTSTJOHN_BCT"          , "BC Transit - Fort St John"               ,
  "BC Transit - Powell River Regional Transit System"      , "CA_BC_POWELLRIVER_BCT"         , "BC Transit - Powell River"               ,
  "BC Transit - Vernon Regional Transit System"            , "CA_BC_VERNON_BCT"              , "BC Transit - Vernon"                     ,
  "BC Transit - West Kootenay Transit System"              , "CA_BC_WESTKOOTENAY_BCT"        , "BC Transit - West Kootenay"              ,
  "BC Transit - South Okanagan Similkameen Transit System" , "CA_BC_SOUTHOKANAGAN_BCT"       , "BC Transit - South Okanagan Similkameen" ,
  "BCTransit"                                              , "CA_BC_GENERIC_BCT"             , "BC Transit"                              ,

  # ========== VANCOUVER REGION (BC) ==========

  "TransLink"                                              , "CA_BC_VANCOUVER_TRANSLINK"     , "TransLink"                               ,
  "West Coast Express"                                     , "CA_BC_VANCOUVER_WCE"           , "West Coast Express"                      ,
  "British Columbia Rapid Transit Company"                 , "CA_BC_VANCOUVER_SKYTRAIN"      , "SkyTrain"                                ,

  # ========== BC FERRIES & WATER TRANSIT ==========

  "BC Ferries"                                             , "CA_BC_FERRIES"                 , "BC Ferries"                              ,
  "Aquabus"                                                , "CA_BC_VANCOUVER_AQUABUS"       , "Aquabus"                                 ,
  "Hullo Ferries"                                          , "CA_BC_HULLO_FERRIES"           , "Hullo Ferries"                           ,
  "Denman Island Bus Service"                              , "CA_BC_DENMAN_BUS"              , "Denman Island Bus"                       ,
  "Gabriola Community Shuttle"                             , "CA_BC_GABRIOLA_SHUTTLE"        , "Gabriola Shuttle"                        ,
  "Hornby Island Bus"                                      , "CA_BC_HORNBY_BUS"              , "Hornby Island Bus"                       ,
  "West Coast Trail Express"                               , "CA_BC_INTERCITY_WCTE"          , "West Coast Trail Express"                ,

  # ========== NIAGARA REGION (ON) ==========

  "Niagara Falls Transit"                                  , "CA_ON_NIAGARA_NFT"             , "Niagara Falls Transit"                   ,
  "Niagara Falls Transit & WEGO"                           , "CA_ON_NIAGARA_NFT"             , "Niagara Falls Transit"                   ,
  "Niagara Falls Transit & WeGo"                           , "CA_ON_NIAGARA_NFT"             , "Niagara Falls Transit"                   ,
  "WEGO"                                                   , "CA_ON_NIAGARA_WEGO"            , "WEGO"                                    ,
  "Niagara Parks Commission WeGo"                          , "CA_ON_NIAGARA_WEGO"            , "WEGO"                                    ,
  "Fort Erie Transit"                                      , "CA_ON_NIAGARA_FORTERIE"        , "Fort Erie Transit"                       ,
  "Town of Fort Erie Transit System"                       , "CA_ON_NIAGARA_FORTERIE"        , "Fort Erie Transit"                       ,
  "Lincoln uLinc Transit"                                  , "CA_ON_NIAGARA_LINCOLN"         , "Lincoln Transit"                         ,
  "uLinc - Lincoln"                                        , "CA_ON_NIAGARA_LINCOLN"         , "Lincoln Transit"                         ,
  "Niagara-on-the-Lake Transit"                            , "CA_ON_NIAGARA_NOTL"            , "Niagara-on-the-Lake Transit"             ,
  "Niagara on the Lake Transit"                            , "CA_ON_NIAGARA_NOTL"            , "Niagara-on-the-Lake Transit"             ,
  "Niagara Region Transit"                                 , "CA_ON_NIAGARA_NRT"             , "Niagara Region Transit"                  ,
  "Transit Systems of Niagara"                             , "CA_ON_NIAGARA_TSN"             , "Transit Systems of Niagara"              ,
  "St. Catharines Transit Commission"                      , "CA_ON_NIAGARA_STCATH"          , "St. Catharines Transit"                  ,
  "Pelham Transit"                                         , "CA_ON_NIAGARA_PELHAM"          , "Pelham Transit"                          ,
  "Welland Transit"                                        , "CA_ON_NIAGARA_WELLAND"         , "Welland Transit"                         ,

  # ========== EDMONTON REGION (AB) ==========

  "Edmonton Transit Service"                               , "CA_AB_EDMONTON_ETS"            , "Edmonton Transit"                        ,
  "Edmonton Transit System"                                , "CA_AB_EDMONTON_ETS"            , "Edmonton Transit"                        ,
  "St. Albert Transit"                                     , "CA_AB_STALBERT_SAT"            , "St. Albert Transit"                      ,
  "Strathcona County Transit"                              , "CA_AB_STRATHCONA_SCT"          , "Strathcona County Transit"               ,
  "Strathcona County - Transit"                            , "CA_AB_STRATHCONA_SCT"          , "Strathcona County Transit"               ,
  "Fort Saskatchewan Transit"                              , "CA_AB_FORTSASK_FST"            , "Fort Saskatchewan Transit"               ,
  "Spruce Grove Transit"                                   , "CA_AB_SPRUCEGROVE_SGT"         , "Spruce Grove Transit"                    ,
  "Beaumont Transit"                                       , "CA_AB_BEAUMONT_BT"             , "Beaumont Transit"                        ,
  "Leduc Transit"                                          , "CA_AB_LEDUC_LT"                , "Leduc Transit"                           ,

  # ========== TORONTO REGION (ON) ==========

  "GO Transit"                                             , "CA_ON_TORONTO_GO"              , "GO Transit"                              ,
  "TTC"                                                    , "CA_ON_TORONTO_TTC"             , "TTC"                                     ,
  "UP Express"                                             , "CA_ON_TORONTO_UP"              , "UP Express"                              ,

  # ========== GREATER TORONTO AREA (ON) ==========

  "Brampton Transit"                                       , "CA_ON_BRAMPTON_BT"             , "Brampton Transit"                        ,
  "Burlington Transit"                                     , "CA_ON_BURLINGTON_BT"           , "Burlington Transit"                      ,
  "Durham Region Transit"                                  , "CA_ON_DURHAM_DRT"              , "Durham Region Transit"                   ,
  "MiWay"                                                  , "CA_ON_MISSISSAUGA_MIWAY"       , "MiWay"                                   ,
  "Milton Transit"                                         , "CA_ON_MILTON_MT"               , "Milton Transit"                          ,
  "Oakville Transit"                                       , "CA_ON_OAKVILLE_OT"             , "Oakville Transit"                        ,
  "Oakville"                                               , "CA_ON_OAKVILLE_OT"             , "Oakville Transit"                        ,
  "York Region Transit"                                    , "CA_ON_YORK_YRT"                , "YRT"                                     ,

  # ========== WATERLOO REGION (ON) ==========

  "Grand River Transit"                                    , "CA_ON_WATERLOO_GRT"            , "Grand River Transit"                     ,
  "grt"                                                    , "CA_ON_WATERLOO_GRT"            , "Grand River Transit"                     ,

  # ========== HAMILTON (ON) ==========

  "Hamilton Street Railway"                                , "CA_ON_HAMILTON_HSR"            , "HSR"                                     ,

  # ========== OTTAWA (ON) ==========

  "OC Transpo"                                             , "CA_ON_OTTAWA_OCT"              , "OC Transpo"                              ,

  # ========== SOUTHWESTERN ONTARIO ==========

  "London Transit"                                         , "CA_ON_LONDON_LT"               , "London Transit"                          ,
  "Transit Windsor"                                        , "CA_ON_WINDSOR_TW"              , "Transit Windsor"                         ,
  "Guelph Transit"                                         , "CA_ON_GUELPH_GT"               , "Guelph Transit"                          ,
  "Sarnia Transit"                                         , "CA_ON_SARNIA_ST"               , "Sarnia Transit"                          ,
  "St. Thomas Transit"                                     , "CA_ON_STTHOMAS_STT"            , "St. Thomas Transit"                      ,
  "Stratford Transit"                                      , "CA_ON_STRATFORD_ST"            , "Stratford Transit"                       ,
  "Ride CK"                                                , "CA_ON_CHATHAMKENT_RCK"         , "Ride Chatham-Kent"                       ,

  # ========== EASTERN ONTARIO ==========

  "Kingston Transit"                                       , "CA_ON_KINGSTON_KT"             , "Kingston Transit"                        ,
  "Cornwall Transit"                                       , "CA_ON_CORNWALL_CT"             , "Cornwall Transit"                        ,
  "Belleville Transit"                                     , "CA_ON_BELLEVILLE_BT"           , "Belleville Transit"                      ,

  # ========== CENTRAL ONTARIO ==========

  "Barrie Transit"                                         , "CA_ON_BARRIE_BT"               , "Barrie Transit"                          ,
  "Orillia Transit"                                        , "CA_ON_ORILLIA_OT"              , "Orillia Transit"                         ,
  "Midland Transit"                                        , "CA_ON_MIDLAND_MT"              , "Midland Transit"                         ,
  "Lindsay Transit"                                        , "CA_ON_LINDSAY_LT"              , "Lindsay Transit"                         ,
  "Orangeville Transit"                                    , "CA_ON_ORANGEVILLE_OT"          , "Orangeville Transit"                     ,
  "Simcoe County Linx"                                     , "CA_ON_SIMCOECOUNTY_LINX"       , "Simcoe County Linx"                      ,

  # ========== NORTHERN ONTARIO ==========

  "Greater Sudbury Transit"                                , "CA_ON_SUDBURY_GST"             , "Greater Sudbury Transit"                 ,
  "Thunder Bay Transit"                                    , "CA_ON_THUNDERBAY_TBT"          , "Thunder Bay Transit"                     ,
  "Sault Ste. Marie Transit"                               , "CA_ON_SAULTSTEMARIE_SMT"       , "Sault Ste. Marie Transit"                ,
  "North Bay Transit"                                      , "CA_ON_NORTHBAY_NBT"            , "North Bay Transit"                       ,
  "Timmins Transit"                                        , "CA_ON_TIMMINS_TT"              , "Timmins Transit"                         ,
  "Temiskaming Transit"                                    , "CA_ON_TEMISKAMING_TT"          , "Temiskaming Transit"                     ,
  "Ontario Northland"                                      , "CA_ON_ONTARIONORTHLAND_ON"     , "Ontario Northland"                       ,

  # ========== OTHER ONTARIO - SPECIALIZED ==========

  "Quinte Access"                                          , "CA_ON_QUINTE_QA"               , "Quinte Access"                           ,
  "OSICARS"                                                , "CA_ON_OSICARS"                 , "OSICARS"                                 ,
  "GOVA Transit"                                           , "CA_ON_GOVA"                    , "GOVA Transit"                            ,
  "TACL"                                                   , "CA_ON_TACL"                    , "TACL"                                    ,
  "T3 Transit"                                             , "CA_ON_INTERCITY_T3"            , "T3 Transit"                              ,
  # ========== CALGARY (AB) ==========

  "Calgary Transit"                                        , "CA_AB_CALGARY_CT"              , "Calgary Transit"                         ,

  # ========== OTHER ALBERTA ==========

  "Roam Transit"                                           , "CA_AB_BANFF_ROAM"              , "Roam Transit"                            ,
  "Grande Prairie Transit"                                 , "CA_AB_GRANDEPRAIRIE_GP"        , "Grande Prairie Transit"                  ,
  "cityofgp"                                               , "CA_AB_GRANDEPRAIRIE_GP"        , "Grande Prairie Transit"                  ,
  "Red Deer Transit"                                       , "CA_AB_REDDEER_RDT"             , "Red Deer Transit"                        ,
  "Lethbridge Transit"                                     , "CA_AB_LETHBRIDGE_LT"           , "Lethbridge Transit"                      ,
  "Medicine Hat Transit"                                   , "CA_AB_MEDICINEHAT_MHT"         , "Medicine Hat Transit"                    ,
  "RMWB"                                                   , "CA_AB_FORTMCMURRAY_RMWB"       , "Fort McMurray Transit"                   ,
  "Hinton Transit"                                         , "CA_AB_HINTON_HT"               , "Hinton Transit"                          ,

  # ========== SASKATCHEWAN ==========

  "Saskatoon Transit"                                      , "CA_SK_SASKATOON_ST"            , "Saskatoon Transit"                       ,
  "Regina Transit"                                         , "CA_SK_REGINA_RT"               , "Regina Transit"                          ,
  "The City of Regina"                                     , "CA_SK_REGINA_RT"               , "Regina Transit"                          ,
  "Prince Albert Transit"                                  , "CA_SK_PRINCEALBERT_PAT"        , "Prince Albert Transit"                   ,
  "City of Moose Jaw"                                      , "CA_SK_MOOSEJAW_MJT"            , "Moose Jaw Transit"                       ,
  "STC"                                                    , "CA_SK_INTERCITY_STC"           , "STC"                                     ,

  # ========== MANITOBA ==========

  "Winnipeg Transit"                                       , "CA_MB_WINNIPEG_WT"             , "Winnipeg Transit"                        ,
  "Brandon Transit"                                        , "CA_MB_BRANDON_BT"              , "Brandon Transit"                         ,

  # ========== NEW BRUNSWICK ==========

  "Codiac Transpo"                                         , "CA_NB_MONCTON_CODIAC"          , "Codiac Transpo"                          ,
  "Saint John Transit"                                     , "CA_NB_SAINTJOHN_SJT"           , "Saint John Transit"                      ,
  "Fredericton Transit"                                    , "CA_NB_FREDERICTON_FT"          , "Fredericton Transit"                     ,
  "Miramichi Transit"                                      , "CA_NB_MIRAMICHI_MT"            , "Miramichi Transit"                       ,

  # ========== NOVA SCOTIA ==========

  "Halifax Transit"                                        , "CA_NS_HALIFAX_HT"              , "Halifax Transit"                         ,
  "Halifax Transit - HRM"                                  , "CA_NS_HALIFAX_HT"              , "Halifax Transit"                         ,
  "Metro Transit - HRM"                                    , "CA_NS_HALIFAX_HT"              , "Halifax Transit"                         ,

  # ========== NEWFOUNDLAND & LABRADOR ==========

  "METROBUS"                                               , "CA_NL_STJOHNS_METROBUS"        , "Metrobus"                                ,
  "Metrobus Transit"                                       , "CA_NL_STJOHNS_METROBUS"        , "Metrobus"                                ,

  # ========== TERRITORIES ==========

  "Whitehorse Transit"                                     , "CA_YT_WHITEHORSE_WT"           , "Whitehorse Transit"                      ,
  "Yellowknife Transit"                                    , "CA_NT_YELLOWKNIFE_YT"          , "Yellowknife Transit"                     ,

  # ========== INTERCITY / NATIONAL ==========

  "VIA Rail"                                               , "CA_NATIONAL_VIA"               , "VIA Rail"                                ,
  "Maritime Bus"                                           , "CA_ATLANTIC_MARITIME"          , "Maritime Bus"                            ,
  "Rider Express"                                          , "CA_PRAIRIES_RIDER"             , "Rider Express"                           ,

  # ========== US CONNECTIONS ==========

  "C-TRAN"                                                 , "US_WA_CLARK_CTRAN"             , "C-TRAN"                                  ,

  # ========== TEST ==========

  "Test Travel Agency"                                     , "TEST_AGENCY"                   , "Test Agency"                             ,
  "Canada/Toronto"                                         , "TEST_AGENCY"                   , "Test Agency"
)

usethis::use_data(gtfs_agency_canonical_map, overwrite = TRUE)
