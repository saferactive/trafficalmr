#' Select the main roads from OSM
#'
#' @param x A data frame of OSM lines
#' @param highway_values Which highway values to use to define 'main' roads?
#' Default includes primary, secondar, trunk, motorway, residential and 'mini_roundabout'
#' road values.
#' @export
#' @family OSM
#' @return Returns an data frame
#'
#' @details The OpenSteetMap contains a lot of detail, this function subsets the
#'   data to just the main roads used by cars by filtering on the highway tag.
#' @examples
#' region_name = "Isle of Wight"
#' # osm = osmextract::oe_get(region_name) # test for IoW
#' # region_name = "Greater London" # test for London
#' osm = tc_data_osm
#' osm_main = osm_main_roads(osm)
#' nrow(osm)
#' nrow(osm_main)
#' nrow(osm_main) / nrow(osm) # keeps ~10-25% of lines
#' plot(osm$geometry, col = "grey")
#' plot(osm_main$geometry, add = TRUE)
osm_main_roads = function(x, highway_values = c("primary","primary_link",
                                                "secondary","secondary_link",
                                                "tertiary","tertiary_link",
                                                "trunk","trunk_link",
                                                "motorway","motorway_link",
                                                "unclassified","residential",
                                                "road","mini_roundabout")){
  x = x[!is.na(x$highway),]
  x = x[x$highway %in% highway_values,]
  x
}


#' Consolidate roads into simplified network
#'
#' @param x a SF data frame of OSM linestrings with projected CRS
#' @param segment numeric, max length of segments in metres
#' @export
#' @family OSM
#' @return Returns an data frame of LINESTRINGS
#'
#' @details This function simplifies a OSM road network by: 1) grouping small
#'   road segments into single names roads 2) splitting long roads into sections
#'   are defined by `segment` 3) Casting MULTILINESTRINGs into LINESTRING
#'
#'   Note: to avoid splitting short roads, roads are only split once they are 2x
#'   `segment`, but are then splits into lengths as defined by `segment`. For
#'   example a 600m road will not be split, but a 1100m road will be split into
#'   approximately 500m segments when `segment = 500`.
#' @examples
#' library(sf)
#' osm = osm_main_roads(tc_data_osm)
#' x = sf::st_transform(osm, 27700)
#' osm_consolidated_200m = osm_consolidate(x, segment = 200)
#' nrow(x)
#' nrow(osm_consolidated_200m) / nrow(x) # fewer lines
#' table(x$name)
#' table(osm_consolidated_200m$name)
#' plot(x$geometry, col = 1:nrow(x))
#' plot(osm_consolidated_200m$geometry, col = 1:nrow(osm_consolidated_200m))
#' summary(sf::st_length(x))
#' summary(sf::st_length(osm_consolidated_200m))
osm_consolidate = function(x, segment = 500){
  if(sf::st_is_longlat(x)){
    stop("Must use projected coordinates")
  }
  n = names(x)
  stopifnot(all(c("name", "ref", "highway") %in% n))
  x = x[, c("name", "ref", "highway")]
  # Group
  x = dplyr::group_by(x, name, ref, highway)
  x = dplyr::summarise(x, do_union = FALSE)
  # merge MULITLINESTRING into LINESTRING
  xls = x[sf::st_geometry_type(x) == "LINESTRING",]
  xmls = x[sf::st_geometry_type(x) == "MULTILINESTRING",]
  xmls = sf::st_line_merge(xmls)
  xmlsA = xmls[sf::st_geometry_type(xmls) == "LINESTRING",]
  xmlsB = xmls[sf::st_geometry_type(xmls) == "MULTILINESTRING",]
  xmlsB = sf::st_cast(xmlsB, "LINESTRING")
  x_merge = list(xls, xmlsA, xmlsB)
  x_merge = dplyr::bind_rows(x_merge)

  x_merge$length = as.numeric(sf::st_length(x_merge))
  x_lth = x_merge$length > (2 * segment)
  x_long = x_merge[x_lth, ]
  x_short = x_merge[!x_lth, ]

  x_long = line_segment_sf(x_long, segment_length = segment)
  res = rbind(x_short,x_long)
  res$length = NULL
  res$length = as.numeric(sf::st_length(res))
  return(res)
}

#' Extract junction points from OSM road linestrings
#'
#' @param x a SF data frame of OSM linestrings
#' @export
#' @family OSM
#' @return Returns an SF data frame of POINTS
#'
#' @details This function finds all junction points in a road network, i.e.
#'   where two roads meet. It excludes road crossings e.g. bridges.
#' @examples
#' library(sf)
#' x = osm_main_roads(tc_data_osm)
#' junctions = osm_get_junctions(x)
#' plot(x$geometry, col = "grey")
#' plot(junctions, add = TRUE)
osm_get_junctions = function(x){
  points = sf::st_cast(x, "MULTIPOINT")
  points = points$geometry
  points = sf::st_cast(points,"POINT")
  # TO be a junction their must be duplication of points
  dup = duplicated(points)
  points = points[dup]
  # But we only want on version of the junction
  dup = duplicated(points)
  points = points[!dup]
  return(points)
}

#' Cluster junction points into polygons
#'
#' @param x a SF data frame of joints
#' @param dist buffer distance past to sf::st_buffer
#' @param nQuadSegs how many segments per quatrant? 5 is the default.
#' @export
#' @family OSM
#' @return Returns an SF data frame of POLYGONS
#'
#' @details This function clusters points together and defines junction
#'   polygons, the size of the polygons is dictated by `dist`. For single
#'   junction points a circle around the junction point is returned. For
#'   clustered junctions a polygon enclosing the whole junction area is
#'   returned. A column called junction_ids provides a looup list between the
#'   junction clusters and the junction points.
#' @examples
#' x = osm_main_roads(tc_data_osm)
#' junctions = osm_get_junctions(x)
#' junction_polygons_15 = cluster_junction(junctions)
#' junction_polygons_30 = cluster_junction(junctions, dist = 30)
#' plot(x$geometry, col = "grey")
#' plot(junction_polygons_30, add = TRUE)
#' plot(junction_polygons_15, add = TRUE)
cluster_junction = function(x, dist = 15, nQuadSegs = 3){
  if(sf::st_is_longlat(x)) {
    buff = stplanr::geo_buffer(x, dist = dist, nQuadSegs = nQuadSegs)
  } else {
    buff = sf::st_buffer(x, dist = dist, nQuadSegs = nQuadSegs)
  }
  ints = sf::st_intersects(buff)
  message(paste0("Clustering ",length(ints)," junctions"))
  ints_clus = cluster_ints(ints)
  ints_clus = ints_clus[lengths(ints_clus) > 0]

  message(paste0("Creating ",length(ints_clus)," geometries"))
  geoms = list()
  pb = progress::progress_bar$new(total = length(ints_clus))
  for(i in seq_len(length(ints_clus))){
    pb$tick()
    sub = buff[ints_clus[[i]]]
    if(length(sub) == 1){
      geoms[[i]] = sub[[1]]
    } else {
      geoms[[i]] = sf::st_union(sub)[[1]]
    }

  }
  attributes(geoms) = attributes(buff)
  res = data.frame(cluster_id = seq(1, length(geoms)))
  res$junction_ids = ints_clus
  res$geometry = geoms
  res = sf::st_sf(res, crs = sf::st_crs(x))
  return(res)
}

#' Internal Function
#'
#' @param x list of intersections from sf::st_intersects
#' @noRd
#' @family internal
#' @return Returns integer
#' @details Identifies clusters of intersecting geometries objects
cluster_ints = function(x){
  res = list()
  pb = progress::progress_bar$new(total = length(x))
  for(i in seq_len(length(x))){
    pb$tick()
    if(length(x[[i]]) == 1){
      if(!is.na(x[[i]])){
        # Single Junction
        res[[i]] = x[[i]]
      }
      # Else move on
    } else {
      # Multi-Junction Cluster
      sub = recursive_ints(x[[i]], x = x)
      res[[i]] = sub
      x[sub] = NA
    }
  }
  return(res)
}

#' Internal Function
#'
#' @param sub integer
#' @param x list of integer
#' @noRd
#' @family internal
#' @return integer
#' @details recursively searches a list for values
recursive_ints = function(sub, x){
  sub = unique(sub)
  sub = sub[order(sub)]

  sub2 = unique(unlist(x[sub]))
  sub2 = sub2[order(sub2)]

  if(identical(sub, sub2)){
    return(sub)
  } else {
    return(recursive_ints(sub2, x = x))
  }

}

#' Fast search for the nearest point to another set of points
#'
#' @param x a SF data frame of POINTS
#' @param y a SF data frame of POINTS
#' @param clusters a list of integers, default NULL
#' @export
#' @family OSM
#' @return Returns an list of ids and distances
#'
#' @details If `clusters` is null will find the nearest y point for each x point
#'   and return a list of two vectors indexes of y length(x) and a distances
#'   length(x). If `clusters` is a list e.g. from `cluster_junction` then the
#'   indexes are replaced with the matching indexes of `clusters`
#' @examples
#' \dontrun{
#' junctions = osm_get_junctions(osm)
#' junction_clusters = cluster_junction(junctions)
#'
#' # Find the nearest junction
#' near_junction = nn_point(crash_junction, junctions)
#'
#' # Find the nearest junction cluster
#' near_cluster = nn_point(crash_junction, junctions, clusters = junction_clusters$junction_ids)
#' }
#'
nn_point = function(x, y, clusters = NULL){
  if(sf::st_is_longlat(x)){
    stop("x must use projected coordinates")
  }
  if(sf::st_is_longlat(y)){
    stop("y must use projected coordinates")
  }
  if(!"sfc_POINT" %in% class(sf::st_geometry(x))){
    stop("x must be POINT")
  }
  if(!"sfc_POINT" %in% class(sf::st_geometry(y))){
    stop("y must be POINT")
  }
  x = sf::st_coordinates(x)
  y = sf::st_coordinates(y)

  nn = nabor::knn(y, x, k = 1, eps = 0, searchtype = 1L, radius = 0)
  dist = as.numeric(nn$nn.dists)
  indx = as.integer(nn$nn.idx)

  if(is.null(clusters)){
    result = list(point_index = indx,
                   distance = dist)
    return(result)
  }

  lookup = rep(seq_along(clusters), vapply(clusters, length, FUN.VALUE = 1L))
  indx_clus = lookup[match(indx, unlist(clusters))]

  warning("Returned distacnes are to points not clusters")
  result = list(cluster_index = indx_clus,
                 distance = dist)
  return(result)


}

#' Fast search for the nearest lines to a set of points
#'
#' @param point a SF data frame of POINTS with projected CRS
#' @param lines a SF data frame of lines with projected CRS
#' @param k integer how many lines to search
#' @param ncores integer how many cores to use in parallel processing, default =
#'   1
#' @export
#' @family OSM
#' @return Returns an list of ids and distances
#'
#' @details The nearest line to a point is a non-trivial calculation which in
#'   theory requires checking the distance to every possible line. This function
#'   takes a shortcut by measuring distances to the centroids of lines, and then
#'   cross checking only nearby lines. The number of lines checked is defined by
#'   `k`. This process cannot guarantee 100% accuracy by is many orders of
#'   magnitude faster. To increase accuracy increase `k`.
#' @examples
#' \dontrun{
#' crash_road_nn = nn_line(crash_road, osm, ncores = 5)
#' }
nn_line = function(point, lines, k = 50, ncores = 1){
  if(sf::st_is_longlat(point)){
    stop("point must use projected coordinates")
  }
  if(sf::st_is_longlat(lines)){
    stop("lines must use projected coordinates")
  }
  if(!"sfc_POINT" %in% class(sf::st_geometry(point))){
    stop("point must be POINT")
  }
  lines = sf::st_geometry(lines)
  point = sf::st_geometry(point)
  cents = sf::st_centroid(lines)
  cents = sf::st_coordinates(cents)
  message(paste0(Sys.time()," finding approximate distance for nearest ",k," centroids"))
  nn = nabor::knn(cents, sf::st_coordinates(point), k = 50, eps = 0, searchtype = 1L, radius = 0)
  nn = nn$nn.idx
  nn = split(nn, 1:nrow(nn))

  message(paste0(Sys.time()," preparing inputs"))
  input = purrr::pmap(.l = list(nn, point), z = lines, crs = sf::st_crs(point), .f = function(x,y,z,crs){
    list(nn = x,
         point = sf::st_sfc(y, crs = crs),
         lines = z[x])
  })
  input = unname(input)

  message(paste0(Sys.time()," measuring exact distances for nearest ",k," lines"))
  cl = parallel::makeCluster(ncores)
  result = pbapply::pblapply(input,
                           FUN = nn_int,
                           cl = cl)

  parallel::stopCluster(cl)
  rm(cl)

  idx = unlist(lapply(result, `[[`, 1))
  dist = unlist(lapply(result, `[[`, 2))
  res = list(idx = idx, dist = dist)
  return(res)
}

#' Internal Function
#'
#' @param sub list of lists
#' @noRd
#' @family internal
#' @return list
#' @details for checking line point distance
nn_int = function(sub){
  dists = as.numeric(sf::st_distance(sub$point, sub$lines))
  dists_min = min(dists)
  idx = sub$nn[dists == dists_min]
  idx = idx[1] # some case of equal distance
  res = list(idx = idx,
              dist = dists_min)
  return(res)

}
#' Break a line into segments
#'
#' @param l line
#' @param n_segments number of segments
#' @param segment_length segment length
#' @export
#' @family OSM
#' @return list
#' @details see stplanr::line_segment note: does not make perfect breaks
line_segment_sf = function(l, n_segments, segment_length = NA) {
  if (!is.na(segment_length)) {
    l_length = as.numeric(sf::st_length(l))
    n_segments = round(l_length / segment_length)
  }

  attrib = sf::st_drop_geometry(l)
  geom = sf::st_geometry(l)

  attrib = attrib[rep(seq(1,nrow(attrib)), times = n_segments),]

  split_int = function(i){
    ln = geom[i]
    n = n_segments[i]
    pts = sf::st_cast(sf::st_sfc(ln),"POINT")
    lth = length(pts)
    brks = seq_len(lth)[!duplicated(ceiling(seq_len(lth)/(lth/n)))]
    brks = brks[seq(2,length(brks))]
    pts = pts[brks]
    res = lwgeom::st_split(ln, pts)
    res = sf::st_collection_extract(res, "LINESTRING")
    return(res)
  }

  geom = pbapply::pblapply(seq(1, length(geom)),
                           FUN = split_int)
  geom = unlist(geom, recursive = FALSE)
  geom = sf::st_as_sfc(geom)

  sf::st_geometry(attrib) = geom
  sf::st_crs(attrib) = sf::st_crs(l)

  return(attrib)
}



