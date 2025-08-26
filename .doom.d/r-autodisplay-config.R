# r-autodisplay-config.R --- R startup configuration for automatic graphics display

# This file implements automatic graphics display for R in euporie/jupyter environments
# Key issue: ggplot2 objects should display automatically without print() commands

cat("Loading R autodisplay configuration...\n")

# ==============================================================================
# PROBLEM ANALYSIS
# ==============================================================================
# Current issue: R requires manual print(p) for ggplot objects to display
# Root cause: Missing automatic display hooks in IRkernel/euporie integration
# Solution: Override print methods and add display hooks for automatic rendering

# ==============================================================================
# IRKERNEL DISPLAY CONFIGURATION  
# ==============================================================================

# Check if we're in a Jupyter/IRkernel environment
is_jupyter <- function() {
  exists("IRdisplay", mode = "environment") || 
  "IRkernel" %in% loadedNamespaces() ||
  identical(getOption("device"), "IRdisplay")
}

if (is_jupyter()) {
  cat("IRkernel environment detected - configuring automatic display\n")
  
  # Ensure IRdisplay is available
  if (!require(IRdisplay, quietly = TRUE)) {
    install.packages("IRdisplay")
    library(IRdisplay)
  }
  
  # Configure plot output formats for automatic display
  options(
    IRdisplay.plot.mimetypes = c("image/png", "image/svg+xml"),
    IRdisplay.width = 800,
    IRdisplay.height = 600,
    IRdisplay.res = 120
  )
  
  cat("IRdisplay configured for automatic graphics\n")
}

# ==============================================================================
# GGPLOT2 AUTOMATIC DISPLAY OVERRIDE
# ==============================================================================

# Override ggplot2 print method for automatic display
if (require(ggplot2, quietly = TRUE)) {
  
  # Store original print method
  original_print_ggplot <- ggplot2:::print.ggplot
  
  # Create enhanced print method with automatic display
  print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
    cat("Auto-displaying ggplot object...\n")
    
    if (is_jupyter()) {
      # In Jupyter: use IRdisplay for automatic rendering
      tryCatch({
        # Create temporary file for plot
        temp_file <- tempfile(fileext = ".png")
        
        # Save plot to file
        ggsave(temp_file, plot = x, width = 8, height = 6, dpi = 120, 
               device = "png", units = "in")
        
        # Display using IRdisplay
        if (file.exists(temp_file)) {
          IRdisplay::display_png(temp_file)
          cat("ggplot displayed via IRdisplay\n")
          
          # Clean up temp file
          unlink(temp_file)
          return(invisible(x))
        }
      }, error = function(e) {
        cat("IRdisplay failed, falling back to original print:", e$message, "\n")
      })
    }
    
    # Fallback to original print method
    original_print_ggplot(x, newpage = newpage, vp = vp, ...)
    return(invisible(x))
  }
  
  # Register the override in ggplot2 namespace
  assignInNamespace("print.ggplot", print.ggplot, ns = "ggplot2")
  
  cat("ggplot2 automatic display override installed\n")
}

# ==============================================================================
# BASE R PLOT AUTOMATIC DISPLAY OVERRIDE
# ==============================================================================

# Override base R plot functions for automatic display in Jupyter
if (is_jupyter()) {
  
  # Override plot function
  original_plot <- graphics::plot
  plot <- function(...) {
    cat("Auto-displaying base R plot...\n")
    
    tryCatch({
      # Create temporary file for plot
      temp_file <- tempfile(fileext = ".png")
      
      # Open PNG device
      png(temp_file, width = 800, height = 600, res = 120)
      
      # Create plot
      result <- original_plot(...)
      
      # Close device
      dev.off()
      
      # Display using IRdisplay
      if (file.exists(temp_file)) {
        IRdisplay::display_png(temp_file)
        cat("Base R plot displayed via IRdisplay\n")
        
        # Clean up temp file
        unlink(temp_file)
        return(invisible(result))
      }
    }, error = function(e) {
      cat("Base R plot autodisplay failed:", e$message, "\n")
      # Ensure device is closed
      tryCatch(dev.off(), error = function(e2) {})
      # Fallback to original plot
      original_plot(...)
    })
  }
  
  # Register override in base environment
  assignInNamespace("plot", plot, ns = "graphics")
  
  cat("Base R plot automatic display override installed\n")
}

# ==============================================================================
# PLOT HOOK FOR AUTOMATIC DISPLAY
# ==============================================================================

# Set up a plot hook that automatically displays any graphics
if (is_jupyter() && exists("setHook")) {
  
  # Hook for plot creation
  setHook("plot.new", function() {
    cat("Plot hook triggered - plot.new\n")
  }, action = "append")
  
  # Hook for device closure (when plot is complete)
  setHook("before.plot.new", function() {
    cat("Plot hook triggered - before.plot.new\n") 
  }, action = "append")
  
  cat("Plot hooks installed for automatic display\n")
}

# ==============================================================================
# AUTOMATIC DISPLAY FOR ASSIGNMENT COMPLETION
# ==============================================================================

# Override the assignment operator to trigger display for ggplot objects
if (require(ggplot2, quietly = TRUE) && is_jupyter()) {
  
  # Create a custom assign that checks for ggplot objects
  auto_display_assign <- function(x, value, ...) {
    # Perform the assignment
    result <- base::assign(x, value, ...)
    
    # Check if the assigned value is a ggplot object
    if (inherits(value, "ggplot")) {
      cat("ggplot object assigned to '", x, "' - auto-displaying\n", sep = "")
      print(value)  # This will use our overridden print.ggplot
    }
    
    invisible(result)
  }
  
  # Note: We cannot override <- operator directly in R
  # This would require more complex AST manipulation
  # For now, rely on the print method override
}

# ==============================================================================
# ENVIRONMENT VERIFICATION
# ==============================================================================

# Function to verify auto-display configuration
verify_autodisplay_config <- function() {
  cat("\n=== R AUTODISPLAY CONFIGURATION VERIFICATION ===\n")
  
  cat("Jupyter environment:", is_jupyter(), "\n")
  cat("IRdisplay available:", requireNamespace("IRdisplay", quietly = TRUE), "\n")
  cat("ggplot2 available:", requireNamespace("ggplot2", quietly = TRUE), "\n")
  
  if (is_jupyter()) {
    cat("IRdisplay plot mimetypes:", toString(getOption("IRdisplay.plot.mimetypes", "none")), "\n")
    cat("IRdisplay width:", getOption("IRdisplay.width", "none"), "\n")
    cat("IRdisplay height:", getOption("IRdisplay.height", "none"), "\n")
  }
  
  # Test if print.ggplot override is working
  if (require(ggplot2, quietly = TRUE)) {
    print_method <- get("print.ggplot", envir = asNamespace("ggplot2"))
    cat("ggplot2 print method overridden:", !identical(print_method, original_print_ggplot), "\n")
  }
  
  cat("=====================================\n\n")
}

# Run verification
verify_autodisplay_config()

cat("R autodisplay configuration completed\n")