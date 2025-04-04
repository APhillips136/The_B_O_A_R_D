if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinyWidgets", quietly = TRUE)) install.packages("shinyWidgets")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")

library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(RSQLite)

####dubious change 
# Establish SQLite Connection
conn <- dbConnect(SQLite(), "jobs.db")

# Create Table if it doesn't exist
dbExecute(conn, "CREATE TABLE IF NOT EXISTS jobs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT,
  description TEXT,
  contact TEXT,
  skills_needed TEXT,
  skills_gained TEXT,
  time_commitment TEXT,
  status TEXT
)")

# Function to load jobs from the database
loadJobs <- function() {
  dbGetQuery(conn, "SELECT * FROM jobs")
}

# Skill options
skill_options <- c("Coding (Basics)", "Coding (Intermediate)", "Coding (Advanced)", 
                   "Presentation Delivery", "Presentation Creation", "Creative",
                   "Economic Appraisal", "Economic Modelling", "Business Case Experience", 
                   "Impact Assessment Experience", "Handling Data", "Excel", "R", "Python", "Other")

# Time commitment options
time_options <- c("1-2 days", "3-5 days", "1-2 weeks", "2-4 weeks", "Long-term project")

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        background-color: #fffACD;
        color: #db7093;
        font-family: 'Courier New', monospace;
        transition: background-color 0.5s, color 0.5s;
        margin: 0; padding: 0; overflow-x: hidden;
      }
      html, body { width: 100%; height: 100%; }
      .container {
        width: 100vw; max-width: 100%; margin: 0 auto; padding: 20px;
        background-color: #fff4b5; border-radius: 10px; box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1);
        display: flex; flex-direction: column; align-items: center; justify-content: center;
      }
      .title {
        font-size: 80px; font-weight: bold; text-align: center; margin-bottom: 20px;
      }
      [data-theme='dark'] body {
        background-color: #000000;
        color: #39ff14;
      }
      [data-theme='dark'] .container {
        background-color: #121212;
        color: #ffffff;
      }
      [data-theme='dark'] .title {
        color: #39ff14 !important;
      }
      [data-theme='dark'] .dataTables_wrapper, [data-theme='dark'] .dataTable {
        color: #39ff14 !important;
        background-color: #000000 !important;
      }
    "))
  ),
  div(class="container",
      div(class="title", "The B.O.A.R.D"),
      switchInput("darkMode", label = "Dark Mode", value = FALSE),
      br(),
      selectInput("filterStatus", "Filter by Status", choices = c("All", "Open", "In Progress", "Completed")),
      dataTableOutput("jobTable"),
      br(),
      textInput("jobTitle", "Job Title", ""),
      textAreaInput("jobDesc", "Job Description", ""),
      textInput("jobContact", "Job Contact", ""),
      
      # Skills Needed Dropdown (Multi-select)
      pickerInput("skillsNeeded", "Skills Needed", choices = skill_options, 
                  multiple = TRUE, options = list(`actions-box` = TRUE)),
      textInput("skillsNeededOther", "Other Skills (if applicable)", ""),
      
      # Skills Gained Dropdown (Multi-select)
      pickerInput("skillsGained", "Skills Gained", choices = skill_options, 
                  multiple = TRUE, options = list(`actions-box` = TRUE)),
      textInput("skillsGainedOther", "Other Skills (if applicable)", ""),
      
      # Time Commitment Dropdown
      selectInput("timeCommit", "Time Commitment", choices = time_options),
      
      selectInput("jobStatus", "Status", choices = c("Open", "In Progress", "Completed")),
      actionButton("postJob", "Post Job", class="btn-success"),
      br(), br(),
      h4("Update Job Status"),
      selectInput("updateJob", "Select a job", choices = character(0)),  
      selectInput("updateStatus", "New Status", choices = c("Open", "In Progress", "Completed")),
      actionButton("update", "Update Status", class="btn-warning"),
      br(), br(),
      h4("Remove a Job"),
      selectInput("removeJob", "Select a job", choices = character(0)),  
      actionButton("remove", "Remove Job", class="btn-danger")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive variable for storing jobs
  jobs <- reactiveVal(loadJobs())
  
  # Show the original welcome modal on app start
  showModal(modalDialog(
    title = "Welcome to The B.O.A.R.D",
    div(style = "font-size: 20px;", "Best.OCE.Analytical.Resource Deployment"),
    tags$hr(),
    h4("Purpose"),
    HTML("<p>In OCE we care deeply about your development. The B.O.A.R.D is designed to help everyone in OCE hit their development goals whilst also making sure resources are allocated in the very most optimal way.<br>
         The B.O.A.R.D lets people with a job that needs doing match up with those who have some spare time for some doing, but need a job.<br>
         Users can post a job alongside some headline information about the job, then others can mosey along and see if they think they'd be suited to the task.</p>"),
    tags$hr(),
    h4("How to Use"),
    HTML("<p><strong>Posting a Job:</strong><br>
         - If you have a job that you want posted to The B.O.A.R.D please first liaise with your line manager.<br>
         - Enter the B.O.A.R.D and provide a job title and necessary details.<br>
         - Click the Post Job button and you're off to the races!<br>
         - Update the job status when progress is made.<br>
         - Once completed, mark it as 'Closed'.</p>
         <p><strong>Taking a Job:</strong><br>
         - Before looking for a job on The B.O.A.R.D, check with your line manager and agree that you have some spare time to pick up jobs.<br>
         - Browse from the array of jobs available and choose one appropriate for you.<br>
         - Contact the job poster directly via email or an instant messaging function.</p>"),
    tags$hr(),
    h4("Contact"),
    HTML("<p>In the unlikely event that your experience with The B.O.A.R.D is less than optimal, please contact example.mdkd@jnfkd.co.uk. We assure you there will be no repercussions for this course of action.</p>"),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
  
  # Observe and update dropdown menus dynamically
  observe({
    job_titles <- jobs()$title
    updateSelectInput(session, "updateJob", choices = job_titles)
    updateSelectInput(session, "removeJob", choices = job_titles)
  })
  
  # Render the job postings table
  output$jobTable <- renderDataTable({
    data <- jobs()
    if (nrow(data) == 0) {
      # Return an empty table if no jobs exist
      return(datatable(data.frame(Title = character(), Description = character(), Status = character()),
                       options = list(dom = 't', pageLength = 5)))
    }
    if (input$filterStatus != "All") {
      data <- data[data$status == input$filterStatus, ]
    }
    datatable(data, options = list(dom = 't', pageLength = 5))
  })
  
  # Post Job with Original Confirmation Text
  observeEvent(input$postJob, {
    showModal(modalDialog(
      title = "Confirm Job Posting",
      HTML("Are you sure you want to add this job?<br> 
           Failure to properly consider this message may have consequences."),
      footer = tagList(
        actionButton("confirmYes_post", "Yes"),
        modalButton("No")
      )
    ))
  })
  
  observeEvent(input$confirmYes_post, {
    removeModal()
    
    # Convert multi-select inputs to strings
    skillsNeeded <- paste(input$skillsNeeded, collapse = ", ")
    skillsGained <- paste(input$skillsGained, collapse = ", ")
    
    # Append "Other" skills if selected
    if ("Other" %in% input$skillsNeeded && input$skillsNeededOther != "") {
      skillsNeeded <- paste(skillsNeeded, input$skillsNeededOther, sep = ", ")
    }
    if ("Other" %in% input$skillsGained && input$skillsGainedOther != "") {
      skillsGained <- paste(skillsGained, input$skillsGainedOther, sep = ", ")
    }
    
    # Insert into database with corrected variable names
    dbExecute(conn, "INSERT INTO jobs (title, description, contact, skills_needed, skills_gained, time_commitment, status) 
                 VALUES (?, ?, ?, ?, ?, ?, ?)",
              params = list(input$jobTitle, input$jobDesc, input$jobContact, 
                            skillsNeeded, skillsGained, input$timeCommit, input$jobStatus))
    jobs(loadJobs())  
  })
  
  # Update Job Status with Original Confirmation Text
  observeEvent(input$update, {
    showModal(modalDialog(
      title = "Confirm Status Update",
      HTML(paste("Are you sure you want to update the status of<strong> ", input$updateJob, "</strong> to <strong>", input$updateStatus, "</strong>?<br> 
           Unauthorized updates may be investigated.")),
      footer = tagList(
        actionButton("confirmYes_update", "Yes"),
        modalButton("No")
      )
    ))
  })
  
  observeEvent(input$confirmYes_update, {
    removeModal()
    
    if (!is.null(input$updateJob) && input$updateJob != "") {
      dbExecute(conn, "UPDATE jobs SET status = ? WHERE title = ?", 
                params = list(input$updateStatus, input$updateJob))
      jobs(loadJobs())  # Refresh job table
    }
  })
  
  # Remove Job with Original Confirmation Text
  observeEvent(input$remove, {
    showModal(modalDialog(
      title = "Confirm Job Removal",
      HTML("Are you sure you want to remove <strong>", input$removeJob, "</strong>? <br>
           Please remember to think before acting."),
      footer = tagList(
        actionButton("confirmYes_remove", "Yes"),
        modalButton("No")
      )
    ))
  })
  
  observeEvent(input$confirmYes_remove, {
    removeModal()
    if (!is.null(input$removeJob) && input$removeJob != "") {
      dbExecute(conn, "DELETE FROM jobs WHERE title = ?", params = list(input$removeJob))
      jobs(loadJobs())  # Refresh job table
    }
  })
  
  # Dark Mode Toggle
  observeEvent(input$darkMode, {
    if (input$darkMode) {
      runjs("document.documentElement.setAttribute('data-theme', 'dark');")
    } else {
      runjs("document.documentElement.removeAttribute('data-theme');")
    }
  })
  
  # Close DB when app stops
  onStop(function() {
    dbDisconnect(conn)
  })
}

shinyApp(ui, server)
