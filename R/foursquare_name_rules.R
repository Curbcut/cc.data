## ============================================================================
##  foursquare_name_rules.R — name checks for Foursquare destination types
##
##  Foursquare categories are right most of the time, but some listings are
##  filed under a category that does not match what the business is: a liquor
##  store under Grocery Store, a bitcoin ATM under Bank, a production company
##  under Movie Theater. These rules catch those by name. They were built and
##  reviewed against every Foursquare name in cc-geodb on 2026-10-20 (English
##  and French, chains and the most frequent independents per type).
##
##  Per type:
##    hard_no   — exclusion groups checked before anything else
##    yes_first — banners that count even if a later exclusion matches
##                ("Costco Wholesale" is a grocery store despite "wholesale")
##    no        — exclusion groups, in order; a group whose name starts with
##                "~" only marks the name unclear and keeps the place
##    yes       — words that read as the type
##  Patterns are PCRE, matched against fsq_norm(name): no accents, lower case,
##  punctuation and apostrophes as spaces ("McDonald's" -> "mcdonald s").
## ============================================================================

#' Name rules for Foursquare destination types
#'
#' @return A named list, one entry per type (`grocery_store`, `pharmacy`, ...),
#'   each with `hard_no`, `yes_first`, `no` (named list of patterns) and `yes`.
#'   See [foursquare_classify_names()].
#' @export
foursquare_name_rules <- function() {
  list(
    grocery_store = list(
      hard_no = c("liquor_beer_wine", "pickup_point"),
      yes_first = paste0(.fsq_rx$supermarket, r"---(|supermarket|supermarche|super marche|grocery|grocer\b|epicerie|\bmarche\b|fruit market|fruiterie)---"),
      no = list(
        `liquor_beer_wine` = r"---(\blcbo\b|liquor|\bsaq\b|\bnslc\b|\banbl\b|alcool|\bbcl\b|beer|\bbiere|brewer|wine|\bvins?\b|vignoble|winery|cellar|spirits|sandbanks|tite frette|dial a bottle|\bvqa\b|brew|cidre|cider|distill|\bsqdc\b|cannabis|\bslga\b|liquor gaming|\bnlc\b|\bpeilcc\b|booze|ale house)---",
        `confectionery_dessert` = r"---(chocolat|laura secord|purdy|candy|bonbon|sweet|confiser|fudge|\bsucre|sugar|diperie|ice cream|creme glacee|gelato|yogourt glace|frozen yogurt|cupcake|donut|beigne|cookie|cinnabon|lindt|godiva|rocky mountain|dairy queen|marble slab|edible arrangement|popcorn|\bnuts?\b|\bnoix\b|kernels|bulk ?barn|\bvrac\b|dessert|erable|maple|sucrerie|truffle|bakery|patisserie|boulangerie)---",
        `convenience` = .fsq_rx$convenience,
        `supplement_health` = r"---(naturiste|\bgnc\b|nutrition house|supplement|vitamin|popeye s supplement|health food|sante naturelle|herboris|produits naturels|natural (?:health|food)|nutrition|protein|healthy planet|nutters|watkins|sangster|magic mushroom|dispensary|pepper palace|hickory farms)---",
        `specialty_food` = r"---(boucher|butcher|\bmeats?\b|viande|charcut|sausage|saucisse|poissonn|\bfish\b|seafood|fruits de mer|fromag|cheese|\bcafe\b|coffee|torref|roaster|espresso|nespresso|\btea\b|davidstea|teaopia|epices|spice|olive|traiteur|catering|m m food market|aliments m m|m m meat|bento sushi|sushi|pasta|deli\b|smoke ?house|jerky|honey|\bmiel\b|kombucha|water\b|\beau\b)---",
        `pickup_point` = r"---(pickup|pick up|ramassage|cueillette|click (?:and|n) collect|pc express|curbside)---",
        `pharmacy_or_other_retail` = paste0(.fsq_rx$pharmacy_banner, r"---(|dollarama|dollar tree|dollar store|canadian tire|home hardware|winners|best buy|staples|ikea|petsmart|pet valu|animal|\bpet\b|florist|fleur|flower|garden|jardin|hardware|quincaill)---"),
        `fuel` = .fsq_rx$fuel,
        `business_or_wholesale` = r"---(distribut|wholesale|grossiste|\bimports?\b|\bimportat|export|food service|foodservice|sysco|gordon food|head office|siege social|brokerage|manufactur|packing|packers|processing|kraft|nestle|hershey|general mills|saputo|agropur|pepsico|coca cola|maple leaf foods|mccain|olymel|lassonde|danone|unilever|mondelez|kellogg|campbell|smucker|lactalis|weston foods|canada bread|herbalife|shaklee|amway|binner|unipac|test merchant|merchant|frito lay|parmalat|concord national|foodhero|supperworks|festa juice|william j walter|rowe farms|reid s dairy|arachides)---",
        `farm_or_market_stand` = r"---(farmer ?s? market|marche (?:public|fermier)|farm stand|kiosque|u pick|\bfarm\b|ferme|orchard|verger|flea market|marche aux puces)---"
      ),
      yes = r"---(epicerie|grocer|supermarche|supermarket|\bmarche\b|\bmarket\b|fruiterie|\bproduce\b|alimentation|food store|food mart|fresh foods?\b|halal|asian food|oriental food|african food|caribbean food|indian grocer|bazaar|\bsuperette\b)---"
    ),
    pharmacy = list(
      hard_no = c("alias_or_department", "manufacturer_distributor_ltc"),
      yes_first = paste0(.fsq_rx$pharmacy_banner, r"---(|phamaprix|phamiliprix|phamasave|pharamasave|lawton|remedy s? ?rx|remedys rx|prescription shop|super thrifty)---"),
      no = list(
        `alias_or_department` = r"---(beauty boutique|wellwise|cosmetic|photo|post office|postes canada|canada post|lottery|loto|health clinic by shoppers|home health ?care|\besso\b|ambulance|red cross)---",
        `manufacturer_distributor_ltc` = r"---(mckesson|medisca|distribut|pharmaceutique?s?\b|pharmaceutical|pharmaceuticals|\blabs?\b|laborator|laboratoire|head office|siege social|affiliees|gennium|apotex|sandoz|pfizer|\bteva\b|pharmascience|\bjamp\b|novartis|bayer|merck|glaxo|astrazeneca|sanofi|\blilly\b|abbvie|roche|biotech|clinical research|\bresearch\b|medical pharmacies group|long term care|\bltc\b|wholesale|compounding supplies|pharmacy services inc|specialty services|infusion|pharmacie centrale|central fill|pharmacies affiliees|pharmacie affiliees a|medisystem|sentrex|kohl frisch|\bcalea\b|nasmark|generex)---",
        `natural_or_other_health` = r"---(naturo|naturiste|health food|vitamin|supplement|herbor|produits naturels|natural|parapharm|veterin|animal|\bpet\b|cannabis|dispensar|weeds|vape|optical|optique|lunett|home health|medical suppl|homecare|home care|orthop|mobility|hearing|travel clinic|massage|chiropr|physio|nutrition|gnc|nature s|alive health|healthy planet|honey ginger)---"
      ),
      yes = paste0(.fsq_rx$pharmacy_banner, r"---(|)---", .fsq_rx$supermarket)
    ),
    clinic = list(
      hard_no = c("cosmetic"),
      yes_first = r"---(urgent|primary care|walk in|sans rendez|family (?:practice|health|medicine|doctors?)|medical (?:centre|center|clinic)|clinique medicale|polyclinique|medicentre|\bgmf\b|\bclsc\b)---",
      no = list(
        `cosmetic` = r"---(esthetique|esthetic|aesthetic|cosmetic|botox|laser hair|med ?spa|beauty)---",
        `lab_imaging_blood` = r"---(lifelabs|life labs|dynacare|cml healthcare|\bbiron\b(?! clinique)|biomedical|alpha lab|laborator|laboratoire|\blabs?\b|blood services|hema quebec|dynalife|bureau veritas|uc baby|\bimaging\b|imagerie|x ray|radiolog|ultrasound|echograph|\bmri\b|irm\b|diagnostic|specimen|collection centre)---",
        `optical_hearing_retail` = r"---(specsavers|\biris\b|vogue optical|pearle vision|new look|optical|optique|lunett|eyewear|eyeglass|glasses|hakim|listenup|hearing ?life|miracle ear|connect hearing|hear right|hearcanada|hearing aid|aides auditives|beltone|amplifon|costco hearing|walmart vision|contact lens|greiche|bonlook|optik|bailey nelson|sonus|hearing solutions|listen up|snore shop|bioped|ortheses|orthese)---",
        `weight_loss_wellness` = r"---(herbal magic|weight watchers|\bww\b|ewyn|herbal one|jm nutrition|weight loss|minceur|\bslim|diet\b|nutrition centre|spa\b|med ?spa|esthetique|esthetic|laser hair|botox|cosmetic|beauty|tanning|wellness centre|float)---",
        `home_care_agency` = r"---(bayshore|we care|comfort keepers|home instead|paramed|victorian order|saint elizabeth|\bvon\b|home care|homecare|home health|soins a domicile|nursing agency|caregiv|nurse next door|spectrum health|carepartners|para ?med|right at home|living assistance)---",
        `residential_care` = r"---(nursing home|retirement|residence|\bmanor\b|long term care|\bltc\b|\blodge\b|\bvilla\b|seniors? (?:home|living|residence)|\bchsld\b|assisted living|hospice|maison de soins|soins palliatifs|care home|personal care home|extendicare|leisureworld|retire at home|revera|chartwell|sienna senior)---",
        `medical_supply_equipment` = r"---(medigas|vitalaire|respiratory|homecare solutions|medical suppl|medical equipment|home medical|mobility|wheelchair|ostomy|\bcpap\b|oxygen|air liquide|proresp|motion specialties|shoppers home health|orthotic|orthopedic suppl|compression|surgical supply|medical products)---",
        `association_charity_admin` = r"---(association|society|societe|foundation|fondation|red cross|croix rouge|st john ambulance|ambulance|paramedic|\bems\b|brokerlink|insurance|assurance|benefits|head office|siege social|alberta health svc|cnib|birthright|community care access|salus|ministry|ministere|government|gouvernement|union\b|syndicat|ordre des|college of)---",
        `pharmacy` = r"---(pharmacy|pharmacie|drug mart|jean coutu|familiprix|uniprix|shoppers)---"
      ),
      yes = r"---(clinic|clinique|medical|medicale?|medecin|\bdoctor|physician|\bmd\b|health cent|centre de sante|community health|\bclsc\b|\bgmf\b|walk in|sans rendez|urgent|family (?:practice|health|medicine|doctors?)|medicentre|appletree|polyclinique|hopital|hospital|dr s office|planned parenthood|travel medicine|physio|chiro|optom|fyidoctors|lifemark|athlete s care|addiction treatment|massage|psycholog|psychotherap|counsel|therap|acupunct|osteopat|naturopath|podiat|foot clinic|speech|audiolog|dietit|kinesi|rehab|mental health|sante mentale|fertility|dermat|cardio|pediatr|obstetr|gynec|orthop|neurolog|surgery|chirurg|lasik|eye (?:care|clinic|doctor)|vision (?:care|centre|clinic)|sleep|allergy|midwi|sage femme|health\b|sante\b|wellness|nurse|infirm)---"
    ),
    allied_health = list(
      hard_no = c("cosmetic"),
      yes_first = r"---(physio|chiro|psycholog|psychotherap|optom|podiat|acupunct|kinesi|osteopat|dietit|nutritionist|counsel|speech|audiolog)---",
      no = list(
        `cosmetic` = r"---(esthetique|esthetic|aesthetic|cosmetic|botox|laser hair|med ?spa|beauty)---",
        `lab_imaging_blood` = r"---(lifelabs|life labs|dynacare|cml healthcare|\bbiron\b(?! clinique)|biomedical|alpha lab|laborator|laboratoire|\blabs?\b|blood services|hema quebec|dynalife|bureau veritas|uc baby|\bimaging\b|imagerie|x ray|radiolog|ultrasound|echograph|\bmri\b|irm\b|diagnostic|specimen|collection centre)---",
        `weight_loss_wellness` = r"---(herbal magic|weight watchers|\bww\b|ewyn|herbal one|jm nutrition|weight loss|minceur|\bslim|diet\b|nutrition centre|spa\b|med ?spa|esthetique|esthetic|laser hair|botox|cosmetic|beauty|tanning|wellness centre|float)---",
        `home_care_agency` = r"---(bayshore|we care|comfort keepers|home instead|paramed|victorian order|saint elizabeth|\bvon\b|home care|homecare|home health|soins a domicile|nursing agency|caregiv|nurse next door|spectrum health|carepartners|para ?med|right at home|living assistance)---",
        `residential_care` = r"---(nursing home|retirement|residence|\bmanor\b|long term care|\bltc\b|\blodge\b|\bvilla\b|seniors? (?:home|living|residence)|\bchsld\b|assisted living|hospice|maison de soins|soins palliatifs|care home|personal care home|extendicare|leisureworld|retire at home|revera|chartwell|sienna senior)---",
        `medical_supply_equipment` = r"---(medigas|vitalaire|respiratory|homecare solutions|medical suppl|medical equipment|home medical|mobility|wheelchair|ostomy|\bcpap\b|oxygen|air liquide|proresp|motion specialties|shoppers home health|orthotic|orthopedic suppl|compression|surgical supply|medical products)---",
        `association_charity_admin` = r"---(association|society|societe|foundation|fondation|red cross|croix rouge|st john ambulance|ambulance|paramedic|\bems\b|brokerlink|insurance|assurance|benefits|head office|siege social|alberta health svc|cnib|birthright|community care access|salus|ministry|ministere|government|gouvernement|union\b|syndicat|ordre des|college of)---",
        `pharmacy` = r"---(pharmacy|pharmacie|drug mart|jean coutu|familiprix|uniprix|shoppers)---"
      ),
      yes = r"---(clinic|clinique|medical|medicale?|medecin|\bdoctor|physician|\bmd\b|health cent|centre de sante|community health|\bclsc\b|\bgmf\b|walk in|sans rendez|urgent|family (?:practice|health|medicine|doctors?)|medicentre|appletree|polyclinique|hopital|hospital|dr s office|planned parenthood|travel medicine|physio|chiro|optom|fyidoctors|lifemark|athlete s care|addiction treatment|massage|psycholog|psychotherap|counsel|therap|acupunct|osteopat|naturopath|podiat|foot clinic|speech|audiolog|dietit|kinesi|rehab|mental health|sante mentale|fertility|dermat|cardio|pediatr|obstetr|gynec|orthop|neurolog|surgery|chirurg|lasik|eye (?:care|clinic|doctor)|vision (?:care|centre|clinic)|sleep|allergy|midwi|sage femme|health\b|sante\b|wellness|nurse|infirm)---"
    ),
    dentist = list(
      hard_no = c(),
      yes_first = NULL,
      no = list(
        `supplier_lab_other` = r"---(medigas|smile ?direct|dental lab|laboratoire dentaire|dental supply|henry schein|patterson dental|sinclair dental|dentsply|whiten|dental ceramics|ceramique dentaire|teeth whitening|dental technolog|insurance|assurance)---",
        `not_dental` = r"---(pharm|optical|optom|physio|chiro|massage|veterin|animal|clinique medicale|medical clinic|walk in)---"
      ),
      yes = r"---(^dr\b|docbraces|dent|orthodont|endodont|periodont|oral (?:and|surgery|maxill)|chirurgie (?:buccale|maxillo)|smile|sourire|denture|denturolog|implant|teeth|tooth)---"
    ),
    gas_station = list(
      hard_no = c("cardlock_bulk", "industrial_gas_propane_heating", "ev_charging"),
      yes_first = paste0(.fsq_rx$fuel, r"---(|canadian tire gas|essence de canadian tire|gas\+|co ?op gas|\bmohawk\b|\bcango\b|flying j|\bxtr\b|hughes|petropass|super gaz|orange ?store|racetrac|huskey|independent at the pumps)---"),
      no = list(
        `auto_service_repair` = r"---(midas|windshield|pare brise|auto repair|garage|mecanique|\btires?\b|\bpneus?\b|car wash|lave auto|lave o|speedy|meineke|jiffy lube|mr lube|oil change|oil changers|towing|remorquage|collision|body shop|auto parts|uniglass|apple auto glass|great canadian oil)---",
        `industrial_gas_propane_heating` = r"---(praxair|air liquide|\blinde\b|messer|welding|soudure|superior propane|propane|bluewave|heating oil|mazout|huile a chauffage|fuel oil|furnace oil|home heating|chauffage|lubricant|lubrifiant|chemical)---",
        `cardlock_bulk` = r"---(cardlock|card lock|commercial|bulk plant|bulk fuel|distribut|wholesale|petroleum ltd|petroleums? inc|energies? inc|head office|siege social|\bterminal\b|depot|imperial oil|exxonmobil|suncor energy|parkland corporation|cst canada|pennzoil|national bank)---",
        `ev_charging` = r"---(charging|recharge|electrify|\bflo\b|supercharger|chargepoint|\bev\b|borne)---",
        `~convenience_brand` = paste0(.fsq_rx$convenience, r"---(|boni ?soir)---"),
        `not_fuel` = r"---(tim hortons)---"
      ),
      yes = paste0(.fsq_rx$fuel, r"---(|canadian tire gas|essence|gas\b|fuel|carburant|petro\b|petrol|co ?op gas|co ?op|\bsonic\b|\beko\b|centex|canco|macewen|tempo|gas plus|race trac|super save|mr gas|costco gas|costco gasoline|superstore gas|safeway gas|petro t\b|\bufa\b|domo|\bgulf\b|north atlantic|esso canada|\bpetroles?\b|\btotal\b|fill up|filling|truck stop|relais routier|travel cent|\bpetro\b)---")
    ),
    bank = list(
      hard_no = c(),
      yes_first = r"---(branch|succursale|centre bancaire|banking centre|banking center|td bank financial group|g f financial|mountain view financial|chinook financial|westminster savings|\byncu\b|\bwfcu\b|peace hills trust|first ?ontario|bcu financial|ic savings|^bnc$)---",
      no = list(
        `payday_cheque_cashing` = r"---(money mart|cash money|cash store|easy ?financial|pay ?2 ?day|payday|insta ?cheques|cash ?4 ?you|\bloans?\b|\bpret|cheque cashing|check cashing|cash canada|title loan|pawn|preteur|credit repair|instaloans|moneymart|cashmoney)---",
        `crypto_atm` = r"---(bitcoin|crypto|coinflip|localcoin|coin ?atm|\bbtm\b|bitbuy)---",
        `atm_only` = r"---(\batm\b|\babm\b|guichet|cash dispenser|argent au guichet|cash machine|guichet automatique)---",
        `money_transfer_fx` = r"---(western union|moneygram|currency|exchange|\bchange\b|\bice\b|calforex|remittance|remit|devises|transfert d argent|ria money)---",
        `wealth_business_insurance` = r"---(dominion securities|wood gundy|wealth|private (?:bank|client|wealth)|investment|placements|valeurs mobilieres|financial planning|securities|insurance|assurance|mortgage|hypothec|commercial banking|business banking|services aux entreprises|entreprises desjardins|corporate|head office|siege social|\bbdc\b|business development|farm credit|financement agricole|\bfcc\b|export development|\bedc\b|trust company|financial group|manulife financial|investors group|edward jones|primerica|h r block|dominion lending|spergel|debt|ecapital|advisor|conseiller|accounting|tax\b|impot|leasing|credit card|visa\b|mastercard|collection agency|agence de recouvrement|pension|annuit)---",
        `kiosk_or_retail` = r"---(president s choice financial|presidents choice financial|pc financial|on the run|co op food|7 eleven|hasty market|grocery|epicerie|pharmacy|depanneur|convenience)---"
      ),
      yes = r"---(scotia|\bbmo\b|bank of montreal|banque de montreal|\brbc\b|royal bank|\bcibc\b|\btd\b|canada trust|banque nationale|national bank|desjardins|caisse|laurentian|laurentienne|\batb\b|servus|meridian|vancity|coast capital|affinity|conexus|\bbeem\b|assiniboine|credit union|caisse populaire|tangerine|eq bank|simplii|manulife bank|motive|\bbank\b|\bbanque\b|canadian western|\bcwb\b|alterna|\bduca\b|firstontario|libro|interior savings|coastal community|g f financial|first west|envision|prospera|steinbach|cambrian|access credit|innovation credit|synergy credit|sunova|crosstown|westoba|connect first|first calgary|kawartha|ukrainian credit|servus|valley first|island savings|blueshore|gulf and fraser|community savings|integris|east kootenay|kootenay savings|northern savings|sharons|bulkley valley|cornerstone credit|copperfin|lakeland credit|vermilion credit|caisse alliance|caisse financial|uni financial|\buni\b|omista|caisse acadienne|east coast credit|league savings|credit unie|teachers plus)---"
    ),
    cinema = list(
      hard_no = c(),
      yes_first = r"---(^cineplex inc$)---",
      no = list(
        `office_production_retail` = r"---(netflix|home entertainment|home theatre|home theater|audio video|electronics|best buy|cineplex odeon corp|cineplex entertainment|cineplex digital|cineplex media|head office|siege social|\bmedia\b|productions?\b|post production|\bvfx\b|animation|distribution|film festival|festival(?! cinema)|film society|rentals?|video store|club video|blockbuster|camera|equipment|sound|recording|casting|talent|agency|school|ecole|academy)---"
      ),
      yes = r"---(\bcine|cinema|movie|imax|drive in|cineplex|landmark|galaxy|silvercity|silver city|famous players|empire|guzzo|rainbow|imagine cinemas|scotiabank theatre|colossus|coliseum|odeon|starlight|cinemark|cinestarz|carrefour du cinema|\bmovies?\b|\bvox\b|cinepark|cine parc|cineparc|ciné)---"
    ),
    cafe = list(
      hard_no = c(),
      yes_first = r"---(tim horton|starbucks|second cup|country style|coffee time|robin s donuts|robins donuts|van houtte(?! (?:inc|coffee serv))|timothy)---",
      no = list(
        `convenience_or_gas` = paste0(.fsq_rx$convenience, r"---(|)---", .fsq_rx$fuel),
        `retail_tea_coffee_equipment` = r"---(nespresso boutique|davidstea|david s tea|davids tea|teavana|teaopia|coffee service|office coffee|vending|distribut|wholesale|espresso machine|equipment|head office|siege social)---",
        `internet_gaming` = r"---(internet cafe|cyber ?cafe|cafe internet|\blan (?:party|cent|gaming)|gaming|e ?sport)---",
        `supermarket_or_other_retail` = paste0(.fsq_rx$supermarket, r"---(|)---", .fsq_rx$pharmacy_banner, r"---(|ikea|indigo|chapters|canadian tire|dollarama)---")
      ),
      yes = r"---(cafe|coffee|caffe|espresso|tim hortons|starbucks|second cup|country style|coffee time|van houtte|presse cafe|timothy|aroma|blenz|jj bean|waves|good earth|bridgehead|serious coffee|\bbean|brew|roast|\btea\b|bubble|boba|chatime|gong cha|\bcoco\b|presotea|real fruit|kung fu tea|sharetea|tiger sugar|the alley|xing fu tang|yifang|palgong|mccafe|muffin|robin s donuts|robins donuts|robins\b|williams fresh|cafe depot|la prep|treats|java|latte|matcha|kava|kafe|kaffee|torref|beanery|perk)---"
    ),
    bakery = list(
      hard_no = c(),
      yes_first = NULL,
      no = list(
        `supermarket_department` = paste0(r"---(grocery pickup|pickup|)---", .fsq_rx$supermarket),
        `industrial_bakery_outlet` = r"---(weston bakeries|canada bread|multi ?marques|bimbo|ben s bakery|\bbens bakery|wonder|dempster|\bpom\b|gadoua|distribut|wholesale|outlet|depot|commercial bakery|boulangerie industrielle|head office|siege social|usine)---",
        `convenience_or_other` = paste0(.fsq_rx$convenience, r"---(|)---", .fsq_rx$pharmacy_banner, r"---(|dollarama|costco|walmart)---")
      ),
      yes = r"---(boulang|bakery|bakeries|bakehouse|bake shop|bakeshop|bread|\bpain\b|patiss|pastry|pastries|viennois|croissant|donut|doughnut|beigne|cobs|premiere moisson|cake|gateau|cupcake|baguette|bagel|tim hortons|robin s donuts|robins donuts|cinnabon|cinnzeo|saint cinnamon|cinnaholic|pretzel|beavertail|queues de castor|crumbl|cookie|biscuit|\bpie\b|tarte|bake|panader|bäckerei|fournil|four a bois|mr puffs|treats)---"
    ),
    bar = list(
      hard_no = c("liquor_retail_events"),
      yes_first = r"---(\bbar\b|\bpub\b|tavern|taverne|sports ?bar|lounge|saloon|brewpub|tap ?room|night ?club)---",
      no = list(
        `~restaurant_with_bar` = r"---(boston pizza|the keg|jack astor|original joe|moxie|kelsey|montana s|state main|chuck s roadhouse|crabby joe|lone star|wild wing|cactus club|earls\b|joey\b|milestones|east side mario|the works|montana|casey s|mito sushi|sushi|pizza|restaurant|resto|burger|grill house|steakhouse|shawarma|poutine|chicken|bbq|diner|pho\b|ramen|thai|indian|chinese|japanese|korean|vietnam|mexican|taco|burrito|noodle|kitchen(?! (?:bar|and bar|pub|tap))|cuisine|bistro(?! bar))---",
        `~legion_or_club_hall` = r"---(royal canadian legion|legion|elks|lions club|knights of columbus|club house|clubhouse|community|hall\b|salle|hotel(?! bar)|motel|inn\b)---",
        `liquor_retail_events` = r"---(paint nite|commission des liqueurs|\bsaq\b|\blcbo\b|liquor store|beer store|wine rack|head office|distribut)---",
        `other_venue` = r"---(paint|bowling|golf|curling|arena|casino|bingo|theatre|theater|events? (?:planning|rental)|catering|traiteur|winery|vineyard)---"
      ),
      yes = r"---(\bbar\b|\bpub\b|tavern|taverne|lounge|saloon|brewhouse|brewpub|tap ?room|\btaps?\b|\bbeer|\bbiere|cocktail|wine bar|bar a vin|bar a vins|night ?club|nightclub|\bclub\b|speakeasy|alehouse|ale house|whisky|whiskey|\bsports? (?:bar|grill)|bar grill|bar and grill|st louis|shoeless joe|fionn|irish|scottish|british|canadian brewhouse|doolys|la cage|buffalo wild|vino volo|karaoke|hookah|shisha|cantina)---"
    ),
    restaurant = list(
      hard_no = c(),
      yes_first = NULL,
      no = list(
        `frozen_food_or_grocery_counter` = r"---(m m food market|aliments m m|m m meat|bento sushi|grocery|epicerie|supermarket|supermarche|^(?:foodland|marche richelieu|rabba fine foods|iga|metro|loblaws|sobeys|safeway|walmart|costco|maxi|provigo|no frills|superstore)$)---",
        `caterer_kitchen_supplier` = r"---(ghost kitchen|cloud kitchen|virtual kitchen|commissary|meal prep|meal kit|hellofresh|chefs plate|goodfood|food service|foodservice|sysco|gordon food|distribut|wholesale|head office|siege social|franchise office|corporate office|enercare|stratacon|compass group|aramark|old dutch|ches s snacks|7 eleven canada)---",
        `convenience_gas_retail` = r"---(^(?:esso|shell|petro canada|husky|7 eleven|couche tard|circle k|ikea|corner store|mac s milk|macs milk|depanneur|dollarama|canadian tire)$)---"
      ),
      yes = r"---(restaurant|resto|pizza|burger|grill|sushi|bistro|kitchen|cuisine|diner|cafe|subway|mcdonald|kfc|a w\b|dairy queen|wendy|burger king|harvey|tim hortons|taco|burrito|pho\b|ramen|thai|shawarma|poutine|chicken|poulet|wings|wing s\b|bbq|smokehouse|steak|noodle|dumpling|pita|sub\b|subs\b|sandwich|deli|chip|fish and chips|fries|frites|casse croute|cantine|food truck|truck|eatery|brasserie|trattoria|pizzeria|taverna|sushi|curry|tandoori|biryani|kebab|kabab|falafel|mezza|dim sum|hot pot|bbq|crepe|creperie|dhaba|buffet)---"
    ),
    gym = list(
      hard_no = c("retail_equipment"),
      yes_first = r"---(crossfit|fitness(?! (?:depot|experience|town|equipment))|\bgym\b|gymnase|yoga|pilates|boxing|martial arts|karate|dojo|strength|exercise)---",
      no = list(
        `retail_equipment` = r"---(peloton|fitness depot|fitness experience|fitness town|treadmill|sports? experts?|sport chek|decathlon|equipment|apparel|clothing|lululemon|home gym)---",
        `nutrition_supplements` = r"---(nutrition|supplement|vitamin)---",
        `health_beauty` = r"---(physio|chiro|massage|clinic|clinique|weight watchers|\bww\b|spa\b|tanning|bronz|salon|hair|coiff|nails?\b|ongles|beauty|esthetique|laser|tattoo|tatou|med ?spa)---",
        `non_fitness_studio_business` = r"---(firelight|solar|photograph|photo studio|recording|music|piano|guitar|art studio|pottery|ceramic|architect|design|media|film|video|productions?\b|consult|marketing|software|church|eglise|college|university|universite|daycare|garderie|real estate|insurance|law\b|avocat|herbal magic)---"
      ),
      yes = r"---(fitness|\bgym|crossfit|yoga|pilates|boxing|\bboxe|martial|karate|taekwondo|tae kwon do|judo|jiu ?jitsu|\bbjj\b|kung fu|muay thai|\bmma\b|dojo|kickbox|\bspin|cycle|barre|orangetheory|anytime|goodlife|planet fitness|fit4less|\bsnap\b|ymca|ywca|econofitness|energie cardio|nautilus|\bf45\b|9round|curves|club pilates|crunch|la fitness|world gym|gold s gym|golds gym|movati|climbing|escalade|bloc\b|boulder|gymnast|cheer|dance|danse|ballet|\bstudio|training|entrainement|bootcamp|boot camp|aikido|tai chi|kendo|fencing|escrime|wrestling|lutte|athletic|conditionnement|cardio|\bfit\b|strength|muscu|weightlifting|powerlifting|hiit|\bhit\b|stretch|zumba|jazzercise|aradia|swim|aquatic|natation|piscine|sport)---"
    )
  )
}

## Shared fragments used by several types.
.fsq_rx <- list(
  supermarket = r"---(\biga\b|\bmetro\b(?! (?:pharmacy|pharmacie))|\bmaxi\b|super c\b|provigo|loblaw|no ?frills|food ?basics|freshco|sobeys|safeway|save on foods|foodland|superstore|zehrs|fortinos|valu ?mart|independent (?:city market|grocer)|your independent|extra foods|farm boy|longo|whole foods|t ?t supermarket|tnt supermarket|h mart|galleria supermarket|supermarche pa\b|\bpa supermarche|marche adonis|\badonis\b|intermarche|inter marche|bonichoix|marches? tradition|richelieu|metro plus|\bavril\b|rachelle bery|segal|akhavan|mourelatos|kim phat|marche hawai|lian tai|thrifty foods|quality foods|buy ?low|nesters|choices market|urban fare|pricesmart|price smart|nations fresh|fresh st\b|co ?op (?:food|grocery|marketplace)|calgary co ?op|sunterra|planet organic|pusateri|highland farms|btrust|al premium|oceans fresh|foody mart|family foods|ag foods|freshmart|valu ?foods|giant tiger|walmart supercent|\bwalmart\b(?! (?:pharmacy|pharmacie|grocery pickup|pickup|vision|photo|tire|auto|optical|garden))|costco(?! (?:pharmacy|gas))|wholesale club|club entrepot|clover farm|supermarches gp|italian centre|federated co ?op|kin s farm market|^co ?op$|^co ?op food|\baxep\b|freson|fruiticana|shop easy|super ?valu|sungiven|bigway|bruno s fine|super sagamie|rabba|denninger|summerhill market|vince s market|\bdominion\b|coleman s|save easy|northmart|northern store|north west company|atlantic superstore|marche ami\b|marche tau|marches tau|super marche|grocery outlet|food town|foodtown|food fair|sobey s|county market|price chopper|independent grocer|real canadian|wholesale foods)---",
  convenience = r"---(couche ?tard|circle k|7 ?eleven|seven eleven|depanneur|\bdep\b|variety|convenience|mini ?mart|\bmac s\b|\bmacs\b|becker|hasty market|quickie|on the run|bonisoir|marche express|\bvoisin\b|daisy mart|winks|needs convenience|mini marche|express mart|corner store|smoke shop|tabagie|tabac|vape|cigar)---",
  fuel = r"---(\besso\b|petro ?canada|\bshell\b|ultramar|\birving\b|husky|chevron|\bmobil\b|sunoco|pioneer|fas gas|gas bar|gas station|station service|\bolco\b|harnois|crevier|petro pass)---",
  pharmacy_banner = r"---(pharm|drug ?mart|drugs?\b|drugstore|apothec|chemist|shoppers|jean coutu|\bpjc\b|familiprix|uniprix|\bproxim|brunet|rexall|\bi d a\b|\bida\b|guardian|medicine shoppe|london drugs|lawtons|remedy s ?rx|remedysrx|pharmasave|centresante|centre sante|horizon sante|inviva|acces pharma|access pharma)---"
)

## Brand keys: aliases (pattern, key), words ignored, and words too generic to key on alone.
.fsq_brand_aliases <- list(
  c(r"---(shoppers|pharmaprix|phamaprix|beauty boutique|wellwise|simply pharmacy)---", "shoppers"),
  c(r"---(jean coutu|\bpjc\b)---", "jeancoutu"),
  c(r"---(\bproxim)---", "proxim"),
  c(r"---(familiprix|phamiliprix)---", "familiprix"),
  c(r"---(uniprix)---", "uniprix"),
  c(r"---(\bbrunet\b)---", "brunet"),
  c(r"---(london drugs)---", "londondrugs"),
  c(r"---(rexall|pharma ?plus)---", "rexall"),
  c(r"---(\bi d a\b|\bida\b)---", "ida"),
  c(r"---(guardian)---", "guardian"),
  c(r"---(medicine shoppe)---", "medicineshoppe"),
  c(r"---(pharmasave|phamasave|pharamasave)---", "pharmasave"),
  c(r"---(pharmachoice)---", "pharmachoice"),
  c(r"---(walmart)---", "walmart"),
  c(r"---(costco)---", "costco"),
  c(r"---(loblaw|real canadian superstore|atlantic superstore|\bzehrs\b|fortinos|provigo|\bmaxi\b|no ?frills|independent grocer|valu ?mart)---", "loblawco"),
  c(r"---(\btd\b|canada trust)---", "td"),
  c(r"---(\brbc\b|royal bank)---", "rbc"),
  c(r"---(\bbmo\b|bank of montreal|banque de montreal)---", "bmo"),
  c(r"---(scotia)---", "scotia"),
  c(r"---(\bcibc\b)---", "cibc"),
  c(r"---(desjardins|\bcaisse)---", "desjardins"),
  c(r"---(banque nationale|national bank|^bnc$)---", "nbc"),
  c(r"---(laurentian|laurentienne)---", "laurentian"),
  c(r"---(cineplex|silver ?city|scotiabank theatre|colossus|famous players|galaxy cinema|coliseum|odeon)---", "cineplex"),
  c(r"---(landmark)---", "landmark"),
  c(r"---(\besso\b)---", "esso"),
  c(r"---(petro ?canada|petro ?pass)---", "petrocan"),
  c(r"---(\bshell\b)---", "shell"),
  c(r"---(ultramar)---", "ultramar"),
  c(r"---(\birving\b)---", "irving"),
  c(r"---(couche ?tard|circle k|\bmac s\b)---", "couchetard"),
  c(r"---(canadian tire)---", "canadiantire"),
  c(r"---(co ?op)---", "coop"),
  c(r"---(tim horton)---", "tims"),
  c(r"---(starbucks)---", "starbucks"),
  c(r"---(second cup)---", "secondcup"),
  c(r"---(m m food market|aliments m m|m m meat)---", "mm"),
  c(r"---(\bsaq\b)---", "saq"),
  c(r"---(\blcbo\b|liquor control board)---", "lcbo"),
  c(r"---(beer store)---", "beerstore"),
  c(r"---(\biga\b)---", "iga"),
  c(r"---(\bmetro\b)---", "metro"),
  c(r"---(sobeys)---", "sobeys"),
  c(r"---(safeway)---", "safeway"),
  c(r"---(save on foods)---", "saveon")
)

.fsq_brand_stop <- c("a", "abm", "and", "atm", "bakery", "bank", "banque", "bar", "boulangerie", "branch", "cafe", "caisse", "canada", "canadian", "center", "centre", "cinema", "cinemas", "clinic", "clinics", "clinique", "co", "corp", "d", "de", "dentaire", "dental", "des", "dr", "drug", "drugs", "drugstore", "du", "epicerie", "et", "express", "extra", "fitness", "gas", "grocery", "gym", "inc", "l", "la", "le", "les", "limited", "ltd", "ltee", "marche", "market", "mart", "pharmacie", "pharmacies", "pharmacy", "plus", "populaire", "pub", "restaurant", "resto", "s", "service", "services", "station", "store", "stores", "supermarche", "supermarket", "svc", "the")

.fsq_brand_generic <- c("big", "blue", "burger", "burgers", "central", "chicken", "chinese", "chiro", "chiropractic", "city", "dentistry", "downtown", "east", "eye", "family", "first", "garden", "golden", "green", "grill", "health", "indian", "japanese", "korean", "little", "main", "medical", "mr", "mrs", "new", "north", "optical", "orthodontics", "park", "pho", "physio", "physiotherapy", "pizza", "pizzeria", "poulet", "red", "royal", "saint", "sante", "shawarma", "smile", "smiles", "south", "st", "sushi", "thai", "vietnamese", "village", "vision", "wellness", "west")

