#' read_snv
#'
#' Read the VBD dataset of [SINAVE](https://www.sinave.gob.mx/)
#'
#' @param path is the path of snv dataset.
#' @param spatial_resolution is the spatial resolution or the administrative level. The values are country, state and municipality.
#' @param des_edo_res is a string for define the state in uppercase.
#' @param des_mpo_res is a string for define the municipality in uppercase.
#' @param estatus_caso 1 probable, 2 confirmado, & 3 descartado.
#'
#' @return a data.table object.
#' @export
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @examples
read_snv <- function(path, spatial_resolution,status_caso, des_edo_res = NULL, des_mpo_res = NULL){

    if(spatial_resolution == "country"){
        data.table::fread(path,
                          header = TRUE,
                          quote = "",
                          #select = vect_cols,
                          fill = TRUE,
                          encoding = "Latin-1") |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(ESTATUS_CASO %in% c(status_caso))

    } else if(spatial_resolution == "state"){
        data.table::fread(path,
                          header = TRUE,
                          quote = "",
                          #select = vect_cols,
                          fill = TRUE,
                          encoding = "Latin-1") |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(ESTATUS_CASO %in% c(status_caso)) |>
            dplyr::filter(DES_EDO_RES %in% c(des_edo_res))

    } else if(spatial_resolution == "municipality"){
        data.table::fread(path,
                          header = TRUE,
                          quote = "",
                          #select = vect_cols,
                          fill = TRUE,
                          encoding = "Latin-1") |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(ESTATUS_CASO %in% c(status_caso)) |>
            dplyr::filter(DES_EDO_RES %in% c(des_edo_res)) |>
            dplyr::filter(DES_MPO_RES %in% c(des_mpo_res))
    } else{

    }


}
