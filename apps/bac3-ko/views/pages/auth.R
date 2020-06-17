Auth = R6Class(
    "Auth",
    inherit = ShinyModule,
    public = list(
        # attributes
        
        # initializer
        initialize = function(){},
        
        # UI
        ui = function(){
            fluidPage(
                shinyjs::useShinyjs(),
                loginUI(id = "login")
            )
        },
        
        # server
        server = function(input, output, session){
            # call the logout module with reactive trigger to hide/show
            logout_init = callModule(
                shinyauthr::logout, 
                id = "logout", 
                active = reactive(.AUTH$credentials()$user_auth)
            )
            
            # call login module supplying data frame, user and password cols
            # and reactive trigger
            .AUTH$credentials = callModule(
                shinyauthr::login, 
                id = "login", 
                data = .AUTH$auth_base,
                user_col = user,
                pwd_col = password,
                log_out = reactive(logout_init())
            )
            
            # pulls out the user information returned from login module
            .AUTH$user_data = reactive({.AUTH$credentials()$info})
        }
    )
)