
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cc.data

<!-- badges: start -->
<!-- badges: end -->

The objective of cc.data is to build and process datasets that are ready
to use in a new Curbcut instance, or to update data in an already
existing Curbcut instance. cc.data offers functions to write and access
the processed data to AWS services such as RDS (Amazon Aurora
Serverless - MySQL) and S3 buckets.

## Installation

You can install the development version of cc.data from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MSSI-urban/cc.data")
```

## Usage

For census data building and processing, cc.data creates and works from
long lists of `tibble`. For faster building, the package frequently uses
functions of the `future_*apply` family from the `future.apply` package,
for any computationally intensive work. These implementations of the
apply family function can be solved using any backend supported by
future (parallelization). We therefore recommend defining a future
backend at the beginning of the data building script.

``` r
future::plan(future::multisession())
```

Let’s also enable the global progression handler to get a progress bar
on those more computationally intensive work.

``` r
progressr::handlers(global = TRUE)
```

#### Database connections

###### Amazon Aurora Serverless - MySQL

To provide the ability to download subsets of the pre-processed data,
cc.data uploads the data into a MySQL serverless database on AWS. To do
so, the builder/uploader must have a database administrator user
account. To read the data, a user account with read privileges only must
be created. To get one, please contact Max at
<maxime.belangerdeblois@mcgill.ca>. The Curbcut team will create a user
account using the `db_create_user` function.

``` r
db_create_user(user, password)
```

Once a user account is created, credentials must be stored as an
environment variable (in the `.Renviron` file which can be opened using
`usethis::edit_r_environ()`.).

``` r
# AWS Curbcut db user with read only privileges
CURBCUT_DB_USER = "user"
CURBCUT_DB_PASSWORD = "password"
```

###### S3 bucket

Every Curbcut instance have its own data folder in which unique
pre-computed datasets are stored for the live application (built through
the `cc.buildr` package). To allow for easier sharing of this and any
other folder, `cc.data` have write/read functions to AWS S3 buckets. To
write to and read from these buckets, a user account must be created
through the AWS console Security credentials \> Access management \>
Users \> Add users. The credential type is
`Access key - Programmatic acess` and attached policies must be either
`AmazonS3FullAccess` to allow writing, or `AmazonS3ReadOnlyAccess` to
only allow reading. Accesses must then be saved in the `.Renviron` file.

``` r
# AWS Curbcut bucket access for the 'x' user
CURBCUT_BUCKET_ACCESS_ID = "access_id"
CURBCUT_BUCKET_ACCESS_KEY = "access_pw"
CURBCUT_BUCKET_DEFAULT_REGION = "us-east-1"
```

## Build and process census data

##### Global cc.data census datasets

There are a set of already built tibbles part of cc.data that, by
default, informs census building and processing functions:

- `cc.data::census_years`: All available census years starting 1996;
- `cc.data::census_scales`: All scales for which data should be built
  and processed (CSD, CT, DA);
- `cc.data::census_vectors_table`: A Curbcut selection of census
  variables or aggregates of census variables. A column for each year of
  the CensusMapper variable names of the census variables to be
  downloaded, as well as for the “parent” variables e.g. in the case
  where a Curbcut variable is a percentage, and the title and
  explanation of the variables.
- `cc.data::census_vectors`: Only the variable codes of the census
  vectors. This is the vector fed to most function arguments asking for
  the census vectors, and it retrieves and subset `census_vectors_table`
  behind the scenes.

Only these can be updated when a new census is out, if we want to add a
scale to the census data building, or if we want to add variables. The
data building and processing will use them by default.

##### Usage

Census data is built and processed through a depth 2 list. The first
depth is the census scales, and the second depth is the census years for
each census scale. For this reason, we can speed up operations by using
two layers of futures. If memory and number of threads allow, the
optimal layers would be for the outer layer to be the same length as the
number of census scales, and the inner layer to be the same length as
the number of census years.

``` r
# Tweak futures
future::plan(list(future::tweak(future::multisession,
                                workers = length(cc.data::census_scales)),
                  future::tweak(future::multisession,
                                workers = length(cc.data::census_years))))
```

##### Census data building and processing workflow

Here is the census data building and processing workfow:

``` r
empty_geometries <- census_empty_geometries()
data_raw <- census_data_raw(empty_geometries)
interpolated <- census_interpolate(data_raw)
normalized <- census_normalize(interpolated)
parent_dropped <- census_drop_parents(normalized)
processed_census <- census_reduce_years(parent_dropped)
```

1.  Download empty geometries, for each census scale and each of their
    census years;
2.  Populate the empty geometries with raw data directly downloaded from
    the `cancensus` package using the variable codes from the
    `census_vectors_table`, as well as the parent variables;
3.  Interpolate all data of all all years to the most recent census
    year.;
4.  Normalize data depending if it is classed as a number or a
    percentage, using the parent variable;
5.  Drop the parent variables;
6.  Reduce all years into once dataframe. The output of the last
    function is a list with only one depth. A list container only the
    number of sf data.frame as there are census scales in
    `cc.data::census_scales`.

##### Census data upload to Amazon Aurora Serverless - MySQL

To provide the ability to download subsets of the pre-processed data, we
store the raw census data of dissemination areas and the processed
census data of all scales to a MySQL database. The raw census data is
used if a builder of a Curbcut instance needs to interpolate census data
to custom boundaries. Interpolating using the raw census data provides
the closest degree of accuracy possible. The two following functions are
tailored to exactly the output of `census_data_raw()$DA` and the output
of `census_reduce_years()`.

``` r
db_write_processed_data(processed_census)
db_write_raw_data(DA_data_raw = data_raw$DA)
```

## Build and process buildings across the country

##### Usage

Most of the operations for the building dataset are performed
iteratively on each province. It is very spatial feature heavy and can
hog a lot of memory. Each worker of the future backend will use between
16 to 20 gb of memory.

##### Download and combine buildings datasets

Our building dataset is built using both OpenStreetMap and the
[Microsoft’s Canadian Building
Footprints](https://github.com/microsoft/CanadianBuildingFootprints) for
places where OSM doesn’t have great coverage. To free up memory between
operations, the following functions write the building dataset for each
province to a destination folder.

``` r
buildings_folder <- "buildings/"

buildings_sf(DA_processed_table = processed_census$DA,
             dest_folder = buildings_folder,
             OSM_cache = TRUE)
```

##### Reverse geocode

The reverse geocoding can be sped up by building our own docker
container of [Nominatim](https://nominatim.org/), which, when mounted
using a [.pbf](http://download.geofabrik.de/north-america/canada.html)
(OpenStreetMap) file, can be used to find the nearest address of a
latitude and longitude coordinate. This option is more than 100x faster
than querying external servers to reverse geocode a large dataset like
all the buildings in the country. The following will download and mount
the docker image:

``` r
rev_geocode_local_nominatim()
```

Using a combination of the [National Open Database of
Addresses](https://www.statcan.gc.ca/en/lode/databases/oda) and the
local instance of Nominatim, we reverse geocode all the buildings in the
country. This takes an even harder toll on memory usage, as the local
Nominatim instance also uses a lot of computational power to serve the
reverse geocoding.

``` r
buildings_rev_geo <- rev_geocode_sf(prov_folder = buildings_folder)
```

##### Buildings upload to Amazon Aurora Serverless - MySQL

The processed building dataset is stored in the MySQL database using
this following piece which stores the table `buildings` with a
`buildings_DA_dict` table. The latter will be used to download subsets
of our building dataset to be downloaded using dissemination area IDs.

``` r
db_write_long_table(df = buildings_rev_geo, tb_name = "buildings")
```
