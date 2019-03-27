# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

# from https://github.com/lme4/lme4/issues/460
library("JuliaCall")
library("lme4")

julia_setup()  # this can take a while
julia_library("MixedModels")
julia_assign("insteval", InstEval)  # copy data.frame to Julia
julia_command("@time fm1 = fit!(LinearMixedModel(@formula(y ~ 1 + service + (1|s) + (1|d) + (1|dept)), insteval),REML=false)")
# refit for timing.  First Julia fit is slower due to Just-In-Time (JIT) compilation
system.time(julia_command("@time fm1 = fit!(LinearMixedModel(@formula(y ~ 1 + service + (1|s) + (1|d) + (1|dept)), insteval));"))
system.time(fm1 <- lmer(y ~ 1 + service + (1|s) + (1|d) + (1|dept), InstEval, REML=FALSE))
summary(fm1)

# Cheating by depending on lme4's modular infrastructure doesn't hurt us that much.
# Here's the breakdown of how long each step takes:

system.time(parsedFormula <- lFormula(formula = y ~ 1 + service + (1|s) + (1|d) + (1|dept),
                                      data = InstEval,
                                      REML=FALSE))
system.time(devianceFunction <- do.call(mkLmerDevfun, parsedFormula))
rho_preopt <- environment(devianceFunction)
system.time(optimizerOutput <- optimizeLmer(devianceFunction))
rho_postopt <- environment(devianceFunction)
system.time(mkMerMod( rho = environment(devianceFunction),
                      opt = optimizerOutput,
                      reTrms = parsedFormula$reTrms,
                      fr = parsedFormula$fr))


jmer <- function(formula, data, REML=TRUE){
    julia_library("MixedModels")
    # how to handle implicit intercept?
    #
    # how are contrasts set via contrasts() handled?
    #
    # how are weights and offsets handled?
    #
    # we also otherwise assume that we're using the subset of the R formula
    # syntax supported by Julia should probably add in some additional checks
    # here.
    jf <- deparse(formula)
    jreml = ifelse(REML, "true", "false")

    julia_assign("jlmerdat",data)
    jcall <- sprintf("jm = fit!(LinearMixedModel(@formula(%s),jlmerdat),REML=%s);",jf,jreml)

    jout <- julia_command(jcall)

    joptimizerOutput <- list(par=julia_eval("jm.optsum.final"),
                         fval=julia_eval("jm.optsum.fmin"),
                         feval=julia_eval("jm.optsum.feval"),
                         # MixedModels.jl doesn't yet implement a lot of the
                         # post-fit convergence checks that lme4 does but a very
                         # crude one is provided by checking whether we reached
                         # iteration stop. Julia has a few good packages for
                         # Automatic Differentiation, maybe it's worthwhile to
                         # use those for the gradient and Hessian checks to
                         # really speed things up?
                         conv=julia_eval("jm.optsum.maxfeval") == julia_eval("jm.optsum.feval"),
                         message=julia_eval("jm.optsum.returnvalue"),
                         optimizer=julia_eval("jm.optsum.optimizer"))


    # we could extract this from the julia object, but the parsing is quite fast
    # in lme4 and then we don't need to worry about converting formats and
    # datatypes

    parsedFormula <- lFormula(formula=formula,
                              data=data,
                              REML=REML)
    # this bit should probably be reworked to extract the julia fields
    devianceFunction <- do.call(mkLmerDevfun, parsedFormula)
    optimizerOutput <- optimizeLmer(devianceFunction,start=joptimizerOutput$par,
                                    control=list(maxeval=1))
    optimizerOutput$feval <- joptimizerOutput$feval
    optimizerOutput$message <- joptimizerOutput$message
    optimizerOutput$optimizer <- joptimizerOutput$optimizer

    rho <- environment(devianceFunction)
    # especially rho$resp seems relevant

    mkMerMod(rho = rho,
            opt = optimizerOutput,
            reTrms = parsedFormula$reTrms,
            fr = parsedFormula$fr)

}

system.time(jm1 <- jmer(y ~ 1 + service + (1|s) + (1|d) + (1|dept), InstEval, REML=FALSE))
summary(jm1)
summary(fm1)
jm1@optinfo
fm1@optinfo
system.time(fm1a <- update(fm1,control=lmerControl(optimizer = "bobyqa")))
summary(fm1a)
fm1a@optinfo

