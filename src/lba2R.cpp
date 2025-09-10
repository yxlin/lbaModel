#include <RcppArmadillo.h>
#include <ggdmcHeaders/lba_simulation.h>
#include <ggdmcHeaders/simulation_type_casting.h>

/*--------------------LBA--------------------*/

//' @rdname First_Passage_Time
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector fptpdf(Rcpp::NumericVector rt_r,
                           Rcpp::NumericMatrix parameter_r,
                           Rcpp::LogicalVector is_positive_drift_r,
                           bool verbose = false)
{
    auto is_positive_drift =
        Rcpp::as<std::vector<bool>>(is_positive_drift_r);
    auto parameters =
        r_mat_to_std_mat<Rcpp::NumericMatrix, double>(parameter_r);

    size_t n_rt = rt_r.size();

    lba::lba_class lba;
    lba.set_parameters(parameters, is_positive_drift);
    if (verbose)
    {
        lba.print_parameters();
    }

    Rcpp::NumericVector out(n_rt);
    for (size_t i = 0; i < n_rt; ++i)
    {
        lba.d(rt_r[i]);
        out[i] = lba.m_pdf;
    }

    return out;
}

//' @rdname First_Passage_Time
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector fptcdf(Rcpp::NumericVector rt_r,
                           Rcpp::NumericMatrix parameter_r,
                           Rcpp::LogicalVector is_positive_drift_r,
                           bool verbose = false)
{
    auto is_positive_drift =
        Rcpp::as<std::vector<bool>>(is_positive_drift_r);
    auto parameters =
        r_mat_to_std_mat<Rcpp::NumericMatrix, double>(parameter_r);

    size_t n_rt = rt_r.size();

    lba::lba_class lba;
    lba.set_parameters(parameters, is_positive_drift);
    if (verbose)
    {
        lba.print_parameters();
    }

    Rcpp::NumericVector out(n_rt);
    for (size_t i = 0; i < n_rt; ++i)
    {
        lba.fptcdf(rt_r[i]);
        out[i] = lba.m_cdf;
    }

    return out;
}

//' @rdname First_Passage_Time
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector n1PDF(Rcpp::NumericVector rt_r,
                          Rcpp::NumericMatrix parameter_r,
                          Rcpp::LogicalVector is_positive_drift_r,
                          bool verbose = false)
{
    std::vector<double> rt_cpp = Rcpp::as<std::vector<double>>(rt_r);
    auto is_positive_drift =
        Rcpp::as<std::vector<bool>>(is_positive_drift_r);

    auto parameters =
        r_mat_to_std_mat<Rcpp::NumericMatrix, double>(parameter_r);

    static lba::lba_class lba;
    lba.set_parameters(parameters, is_positive_drift);
    bool is_valid = lba.validate_parameters(verbose);

    if (verbose || !is_valid)
    {
        lba.print_parameters();
    }
    std::vector<double> out_cpp(rt_r.size(), 0.0);

    if (is_valid)
    {
        out_cpp = lba.dlba(rt_cpp);
    }

    return Rcpp::wrap(out_cpp);
}

//' @rdname lba_distributions
//' @export
// [[Rcpp::export]]
Rcpp::List dlba(Rcpp::NumericVector rt_r, Rcpp::NumericMatrix parameter_r,
                Rcpp::LogicalVector is_positive_drift_r, bool debug = false)
{
    auto parameters =
        r_mat_to_std_mat<Rcpp::NumericMatrix, double>(parameter_r);

    auto rt = Rcpp::as<std::vector<double>>(rt_r);
    auto is_positive_drift = Rcpp::as<std::vector<bool>>(is_positive_drift_r);

    lba::lba_class lba;
    lba.set_parameters(parameters, is_positive_drift);

    if (debug)
    {
        lba.print_parameters();
    }

    auto densities = lba.dlba_all(rt);

    // Convert to R list
    Rcpp::List out;
    for (size_t i = 0; i < densities.size(); ++i)
    {
        out.push_back(Rcpp::wrap(densities[i]));
    }

    return out;
}

//' @rdname lba_distributions
//' @export
// [[Rcpp::export]]
Rcpp::List plba(Rcpp::NumericVector rt_r, Rcpp::NumericMatrix parameter_r,
                Rcpp::LogicalVector is_positive_drift_r,
                Rcpp::NumericVector time_parameter_r, bool debug = false)
{
    auto parameters =
        r_mat_to_std_mat<Rcpp::NumericMatrix, double>(parameter_r);

    // std::vector<std::vector<double>> parameters =
    //     numericMatrixToVector(parameter_r);
    auto rt = Rcpp::as<std::vector<double>>(rt_r);
    auto is_positive_drift = Rcpp::as<std::vector<bool>>(is_positive_drift_r);
    auto time_parameter = Rcpp::as<std::vector<double>>(time_parameter_r);

    // Setting the lba_class to static will crash R session when you run this
    // function repeatedly
    lba::lba_class lba;
    lba.set_parameters(parameters, is_positive_drift);
    lba.set_times(time_parameter);

    if (debug)
    {
        lba.print_parameters();
    }

    auto densities = lba.plba_all(rt);

    // Convert to R list
    Rcpp::List out;
    for (size_t i = 0; i < densities.size(); ++i)
    {
        out.push_back(Rcpp::wrap(densities[i]));
    }

    return out;
}

//*--------------------LBA Inverse Method--------------------*/

//' @rdname rlba
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame rlba_r(Rcpp::NumericMatrix parameter_r,
                       Rcpp::LogicalVector is_positive_drift_r,
                       Rcpp::NumericVector time_parameter_r, unsigned int n = 1,
                       bool use_inverse_method = false, bool debug = false)
{
    auto is_positive_drift = Rcpp::as<std::vector<bool>>(is_positive_drift_r);
    auto parameters =
        r_mat_to_std_mat<Rcpp::NumericMatrix, double>(parameter_r);

    auto time_parameter = Rcpp::as<std::vector<double>>(time_parameter_r);

    lba::lba_class lba;
    lba.set_parameters(parameters, is_positive_drift);
    lba.set_times(time_parameter);
    bool is_valid = lba.validate_parameters(debug);

    if (debug || !is_valid)
    {
        lba.print_parameters();
    }

    std::vector<std::pair<unsigned int, double>> trials(n);
    if (is_valid)
    {
        if (use_inverse_method)
        {
            lba.r_inverse(trials);
        }
        else
        {
            lba.r(trials);
        }
    }

    SimulationResults results(n);

    for (const auto &trial : trials)
    {
        results.add_trial(trial.first, trial.second);
    }

    return Rcpp::DataFrame::create(Rcpp::Named("RT") = results.reaction_times,
                                   Rcpp::Named("R") = results.responses);
}

//' @rdname lba_distributions
//' @export
// [[Rcpp::export]]
Rcpp::List theoretical_dlba(Rcpp::NumericMatrix parameter_r,
                            Rcpp::LogicalVector is_positive_drift_r,
                            Rcpp::NumericVector time_parameter_r, bool debug = false)
{
    auto parameters =
        r_mat_to_std_mat<Rcpp::NumericMatrix, double>(parameter_r);

    auto is_positive_drift = Rcpp::as<std::vector<bool>>(is_positive_drift_r);
    auto time_parameter = Rcpp::as<std::vector<double>>(time_parameter_r);

    lba::lba_class lba;
    lba.set_parameters(parameters, is_positive_drift);
    lba.set_times(time_parameter);

    if (debug)
    {
        lba.print_parameters();
    }

    auto densities = lba.theoretical_dlba();

    // Convert to R list
    Rcpp::List out;
    for (size_t i = 0; i < densities.size(); ++i)
    {
        out.push_back(Rcpp::wrap(densities[i]));
    }

    return out;
}

//' @rdname lba_distributions
//' @export
// [[Rcpp::export]]
Rcpp::List theoretical_plba(Rcpp::NumericMatrix parameter_r,
                            Rcpp::LogicalVector is_positive_drift_r,
                            Rcpp::NumericVector time_parameter_r, bool debug = false)
{
    auto parameters =
        r_mat_to_std_mat<Rcpp::NumericMatrix, double>(parameter_r);

    auto is_positive_drift = Rcpp::as<std::vector<bool>>(is_positive_drift_r);
    auto time_parameter = Rcpp::as<std::vector<double>>(time_parameter_r);

    lba::lba_class lba;
    lba.set_parameters(parameters, is_positive_drift);
    lba.set_times(time_parameter);
    if (debug)
    {
        lba.print_parameters();
    }

    auto densities = lba.theoretical_plba();

    Rcpp::List out;
    for (size_t i = 0; i < densities.size(); ++i)
    {
        out.push_back(Rcpp::wrap(densities[i]));
    }

    return out;
}

//' @export
//' @rdname dlba_inverse_external
// [[Rcpp::export]]
Rcpp::NumericVector
dlba_inverse_external(Rcpp::NumericVector rt_r, Rcpp::IntegerVector response_r,
                      Rcpp::NumericMatrix parameter_r,
                      Rcpp::LogicalVector is_positive_drift_r,
                      Rcpp::NumericVector time_parameter_r)
{
    // Compute log-likelihood for all trials
    auto parameters =
        r_mat_to_std_mat<Rcpp::NumericMatrix, double>(parameter_r);

    auto rts = Rcpp::as<std::vector<double>>(rt_r);
    auto responses = Rcpp::as<std::vector<unsigned int>>(response_r);
    auto is_positive_drift = Rcpp::as<std::vector<bool>>(is_positive_drift_r);
    auto time_parameter = Rcpp::as<std::vector<double>>(time_parameter_r);

    lba::lba_class lba;
    lba.set_parameters(parameters, is_positive_drift);
    lba.set_times(time_parameter);

    auto out_cpp = lba.dlba_inverse(rts, responses);
    return Rcpp::wrap(out_cpp);
}

//' @rdname lba_lowlevel
//' @export
// [[Rcpp::export]]
bool validate_lba_parameters(const Rcpp::S4 &rt_model_r,
                             const Rcpp::NumericVector &parameters_r,
                             bool debug = false)
{
    auto d_ptr = new_design_light_rt_model(rt_model_r);
    size_t nparameter = parameters_r.size();
    if (d_ptr->m_n_free_parameter != nparameter)
    {
        Rcpp::stop("Invalid parameter vector: Expected length " +
                   std::to_string(d_ptr->m_n_free_parameter) + ", but got " +
                   std::to_string(nparameter));
    }

    // TODO remove l_ptr and find m_is_positive_drift in rt_model_r;
    auto l_ptr = new_likelihood_for_simulation(rt_model_r);
    auto parameters = Rcpp::as<std::vector<double>>(parameters_r);

    bool is_valid = false;
    lba::lba_class lba_obj;

    for (size_t cell_idx = 0; cell_idx < d_ptr->m_n_cell; ++cell_idx)
    {
        d_ptr->set_parameter_values(cell_idx, parameters);

        lba_obj.set_parameters(d_ptr->m_parameter_matrix[cell_idx],
                               l_ptr->m_is_positive_drift);
        is_valid = lba_obj.validate_parameters(debug);
        if (!is_valid)
        {
            Rcpp::Rcout << "Invalid LBA parameters at the condition, "
                        << d_ptr->m_cell_names[cell_idx] << ": \n";
            lba_obj.print_parameters();
            break;
        }
    }
    return is_valid;
}

//' @rdname lba_lowlevel
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame simulate_lba_trials(const Rcpp::S4 &rt_model_r,
                                    const Rcpp::NumericVector &parameters_r,
                                    unsigned int n_trial = 1L,
                                    bool use_inverse_method = false,
                                    bool debug = false)
{
    // 1. Set pu a new design instance
    auto d_ptr = new_design_light_rt_model(rt_model_r);
    std::vector<bool> is_positive_drift = rt_model_r.slot("is_positive_drift");

    // 2. Trial Distribution Setup
    unsigned int n_condition = d_ptr->m_n_cell / d_ptr->m_n_accumulator;
    unsigned int n_trial_per_cell = n_trial / n_condition;

    // 3. Simulation
    SimulationResults results(n_trial);
    static lba::lba_class lba_obj;
    auto parameters = Rcpp::as<std::vector<double>>(parameters_r);

    simulate_each_condition(d_ptr, lba_obj, parameters, is_positive_drift,
                            n_trial_per_cell, use_inverse_method, results,
                            debug);

    return new_DataFrame(results);
}
