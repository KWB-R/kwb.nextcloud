# Test generating request bodies
if (FALSE)
{
  # Example requests from https://github.com/dmfs/davwiki/wiki/PROPFIND

  # <?xml version="1.0" encoding="utf-8" ?>
  # <D:propfind xmlns:D="DAV:">
  #   <D:prop xmlns:R="http://ns.example.com/boxschema/">
  #     <R:bigbox/>
  #     <R:author/>
  #     <R:DingALing/>
  #     <R:Random/>
  #   </D:prop>
  # </D:propfind>

  cat(kwb.nextcloud:::request_body(
    kwb.nextcloud:::element_propfind(
      owncloud = FALSE,
      nextcloud = FALSE,
      kwb.nextcloud:::element_prop(
        kwb.nextcloud:::tag_xml("R:bigbox"),
        kwb.nextcloud:::tag_xml("R:author"),
        kwb.nextcloud:::tag_xml("R:DingALing"),
        kwb.nextcloud:::tag_xml("R:Random"),
        attributes = list("xmlns:R" = "http://ns.example.com/boxschema/")
      )
    )
  ))

  # <?xml version="1.0" encoding="utf-8" ?>
  # <propfind xmlns="DAV:">
  #   <propname/>
  # </propfind>

  cat(kwb.nextcloud:::request_body(
    kwb.nextcloud:::element_propfind(
      owncloud = FALSE,
      nextcloud = FALSE,
      kwb.nextcloud:::tag_xml("propname")
    )
  ))

  # <?xml version="1.0" encoding="utf-8" ?>
  # <D:propfind xmlns:D="DAV:">
  #   <D:allprop/>
  # </D:propfind>

  cat(kwb.nextcloud:::request_body(
    kwb.nextcloud:::element_propfind(
      owncloud = FALSE,
      nextcloud = FALSE,
      kwb.nextcloud:::tag_xml("d:allprop")
    )
  ))
}

# request_body_list_files ------------------------------------------------------
request_body_list_files <- function()
{
  request_body(
    element_propfind(
      tag_xml("d:prop"),
      tag_xml("d:getlastmodified"),
      tag_xml("d:getetag"),
      tag_xml("d:getcontenttype"),
      tag_xml("d:resourcetype"),
      tag_xml("oc:fileid"),
      tag_xml("oc:permissions"),
      tag_xml("oc:size"),
      tag_xml("d:getcontentlength"),
      tag_xml("nc:has-preview"),
      tag_xml("oc:favorite"),
      tag_xml("oc:comments-unread"),
      tag_xml("oc:owner-display-name"),
      tag_xml("oc:share-types")
    )
  )
}
