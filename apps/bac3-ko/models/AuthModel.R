AuthModel = R6Class(
    "AuthModel",
    public = list(
        auth_base = NULL,
        user_data = NULL,
        credentials = NULL,
        
        initialize = function(){
            self$auth_base = data.frame(
                user = c("zivkoviclab", "JacopoDiLucente"),
                password = c("zivkoviclab", "JacopoDiLucente")
            )
        },
        # getUserTabs = function(){
        #     if(is.null(self$user_data)) return()
        #     user = self$user_data()$user
        #     tabs = self$auth_base$tabs[[which(self$auth_base$user == user)]]
        #     return(tabs)
        # },
        getUsername = function(){
            if(is.null(self$user_data)) return()
            return(self$user_data()$user)
        }
    )
)
