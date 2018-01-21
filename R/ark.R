ark <- new.env()

#' Query or set the ARK node to use
#' @param node optional ARK node with HTTPS prefix
#' @return string
#' @export
ark_node <- function(node = 'https://api.arknode.net') {
    if (is.null(ark$nethash)) {
        ark$nethash <- node
    }
    ark$nethash
}


#' Get ARK node nethash
#' @return string
#' @export
ark_nethash <- function() {
    content(GET(file.path(ark_node(), 'api/blocks/getNethash')))$nethash
}


#' Get ARK balance
#' @return string
#' @export
#' @param address ARK address
ark_balance <- function(address) {
    as.numeric(content(GET(
        file.path(ark_node(), 'api/accounts/getBalance'),
        query = list(address = address),
        add_headers(nethash = ark_nethash())))$balance)
}
