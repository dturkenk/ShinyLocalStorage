#' Set up a Shiny app to use ShinyLocalStorage
#'
#' This must be called from within a Shiny app's UI.
#'
#' The ShinyLocalStorage Javascript library and dependencies
#' will be inserted as script tags in the page header.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   shinyApp(
#'     ui = fluidPage(
#'       useShinyLocalStorage(),  # Set up ShinyLocalStorage
#'       actionButton("btn", "Click me"),
#'       textInput("element", "Store me locally")
#'     ),
#'     server = function(input, output) {
#'       observeEvent(input$btn, {
#'         # Store the value into the local storage
#'         store("stored_element", input$element)
#'       })
#'     }
#'   )
#' }
#' @export
useShinyLocalStorage <- function() {
    template.loc <- file.path(find.package(package = .packageName), "www")

    htmltools::htmlDependency("shinyLocalStorage",
        "0.1.0",
        src = template.loc,
        script = c("shinyLocalStorage.js", "localforage.min.js")
    )
}

#' Configures the ShinyLocalStorage instance for a given page / app.
#'
#' This must be called from within the server function of a Shiny app.
#' You are not required to configure ShinyLocalStorage, but it is recommended
#' to get a unique data store for each app.
#'
#' @param appName A unique identifier for the app. This is used to ensure that
#'   the ShinyLocalStorage instance is unique to this app.
#' @param session The Shiny session object
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   shinyApp(
#'     ui = fluidPage(
#'       useShinyLocalStorage(),  # Set up ShinyLocalStorage
#'       actionButton("btn", "Click me"),
#'       textInput("element", "Store me locally")
#'     ),
#'     server = function(input, output) {
#'       configureShinyLocalStorage("my_app")
#'       observeEvent(input$btn, {
#'         # Store the value into the local storage
#'         store("stored_element", input$element)
#'       })
#'     }
#'   )
#' }
#' @export
configureShinyLocalStorage <- function(appName, session = getDefaultReactiveDomain()) {
    # Generate a random string to use as a prefix for the stores and input objects
    random_string <- paste0(sample(c(letters, LETTERS, 0:9), 8, replace = TRUE), collapse = "")
    session$userData$shinyLocalStoragePrefix <- glue::glue("shinyLocalStorage_{random_string}")

    session$sendCustomMessage("configureShinyLocalStore", list(appName = appName))
}

#' Stores an item into the local storage.
#'
#' @param key The key to store the value under
#' @param value The value to store
#' @param session The Shiny session object
#'
#' @examples
#' if(interactive()) {
#'  observeEvent(input$btn, {
#'     # Store the value into the local storage
#'     store("stored_element", input$element)
#'  })
#' }
#' @export
store <- function(key, value, session = getDefaultReactiveDomain()) {
    session$sendCustomMessage("store", list(key = key, value = value))
}


#' Retrieves an item from local storage.
#'
#' Stores the retrieved value into the Shiny \code{input} object
#' named using a random prefix and the key, as well as returns it.
#'
#' If \code{configureShinyLocalStorage} has not been called,
#' the value will be stored in the \code{input} object under the key.
#'
#' @param key The key to store the value under
#' @param value The value to store
#' @param session The Shiny session object
#'
#' @return The value stored under the key
#'
#' @examples
#' if(interactive()) {
#'  observeEvent(input$btn, {
#'     # Store the value into the local storage
#'     stored_value <- retrieve("stored_element")
#'  })
#' }
#' @export
retrieve <- function(key, session = getDefaultReactiveDomain()) {
    target <- paste(session$userData$shinyLocalStoragePrefix, key, sep = "_")
    session$sendCustomMessage("retrieve", list(key = key, target = target))

    return(session$input[[target]])
}

#' Clears all items from the local storage.
#'
#' @param session The Shiny session object
#'
#' @examples
#' if(interactive()) {
#'  observeEvent(input$clear, {
#'   clear()
#'  })
#' }
#' @export
clear <- function(session = getDefaultReactiveDomain()) {
    session$sendCustomMessage("clear", list())
}

#' Removes a value from local storage.
#'
#' Note: the namespaced \code{input} object will still exist.
#'
#' @param key The key of the value to remove
#' @param session The Shiny session object
#'
#' @examples
#' if(interactive()) {
#'  observeEvent(input$remove, {
#'    remove("stored_element")
#'  })
#' }
#' @export
remove <- function(key, session = getDefaultReactiveDomain()) {
    session$sendCustomMessage("remove", list(key = key))
}
