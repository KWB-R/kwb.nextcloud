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

  body <- kwb.nextcloud:::request_body(
    kwb.nextcloud:::element_propfind(
      kwb.nextcloud:::tag_xml("propname")
    )
  )

  href <- kwb.nextcloud:::path_to_file_href("projects/finale/urban-systems")

  xml <- nextcloud_request(
    href, verb = "PROPFIND", auth = auth, body = body, as = "parsed"
  )

  cat(as.character(xml))
}
