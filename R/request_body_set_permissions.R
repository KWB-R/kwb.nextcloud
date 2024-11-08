# request_body_set_permissions -------------------------------------------------
#' @examples
#' cat(request_body_set_permissions(
#'   group_info = list(name = "agroup", mask = 16L, permissions = 15L),
#'   user_info = list(name = "anuser", mask = 16L, permissions = 15L)
#' ))
request_body_set_permissions <- function(group_info, user_info)
{
  nc_acl <- function(type, name, mask, permissions) {
    element_xml(
      "nc:acl",
      inline_element_xml("nc:acl-mapping-type", type),
      inline_element_xml("nc:acl-mapping-id", name),
      inline_element_xml("nc:acl-mask", mask),
      inline_element_xml("nc:acl-permissions", permissions)
    )
  }

  paste(collapse = "\n", c(
    paste0("<?xml version=", dq("1.0"), "?>"),
    element_xml(
      "d:propertyupdate",
      attributes = attributes_cloud_urls(),
      element_xml(
        "d:set", element_xml(
          "d:prop", element_xml(
            "nc:acl-list",
            nc_acl("group", group_info$name, group_info$mask, group_info$permissions),
            nc_acl("user", user_info$name, user_info$mask, user_info$permissions)
          )
        )
      )
    )
  ))
}

# code_to_value <- function(code) {
#   sum(c(r = 4L, w = 2L, x = 1L)[strsplit(code, "")[[1L]]])
# }
#
# list(
#   user = code_to_value("rwx"),
#   group = code_to_value("rx"),
#   other = code_to_value("r")
# )
