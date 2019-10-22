#include <vector>

#include "boost/property_map/property_map.hpp"
#include "Rcpp.h"

using namespace Rcpp;


// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}

using patch_id = int;
using human_id = int;
using clock_time = double;
using movement_sequence = std::vector<std::tuple<patch_id,clock_time>>;


/*!
 * The sampling trajectory of the movement machine
 * tells the human when to change biting weight on
 * a patch and where they have hazard of being
 * bitten.
 */
class movement_machine_result {
public:
    patch_id starting_patch(human_id query) const { return 4; };

    size_t human_count() { return 10; }

    /*!
     * For the human, the movement sequence will be a
     * set of events with predetermined times.
     *
     * @param query - Which human's movements we want.
     * @return movement_sequence - The set of patches and times.
     */
    movement_sequence
    movements_of_human(human_id query) const {
        return {{3, 0.02}, {4, 0.05}, {3, 0.8}};
    }

    movement_sequence
    duration_in_patch(human_id query) const {
        return {{4, .77}, {3, .23}};
    }
};


class movement_machine {
    // The result is a buffer that is owned by the machine,
    // so that it won't churn memory. It is read-only to others.
    movement_machine_result result;
public:
    void init() {}

    const movement_machine_result*
    step(double time_step) { return &result; }
};


/*!
 * From R, this will look like
 *     movement <- movement_init(params)
 *     time_step <- 0.1
 *     moves <- movement_step(movement, time_step)
 *     person <- human_model(moves, time_step)
 * @param parameters
 * @return
 */
// [[Rcpp::export]]
List movement_init(List parameters) {
    auto movement = new movement_machine{};
    movement->init();

    XPtr<movement_machine> handle_ptr(movement);
    auto movement_object = List::create(
            Named("handle") = handle_ptr,
            Named("parameters") = parameters
            );
    movement_object.attr("class") = CharacterVector::create("MovementModel", "MovementModel");
    return movement_object;
}


// [[Rcpp::export]]
List movement_step(List module, NumericVector time_step) {
    auto movement_machine_handle = as<XPtr<movement_machine>>(module["handle"]);
    auto result = movement_machine_handle->step(as<double>(time_step));
    return List::create(
            Named("handle") = XPtr<const movement_machine_result>(result)
            );
}


/*!
 * This approach copies all data out of C++ into the R space because it's
 * the transition from R - C++ and back that takes all the time.
 *
 * @param movement_list - This gets modified in place, so that it now has
 *     a list of trajectories for each person, enumerated by the person id.
 */
// [[Rcpp::export]]
void convert_to_r_movement(List movement_list) {
    auto result = as<XPtr<const movement_machine_result>>(movement_list["handle"]);
    auto moves = List::create();
    for (int person_idx = 0; person_idx < result->human_count(); ++person_idx) {
        auto sequence = result->movements_of_human(human[0]);
        auto vector = NumericVector(Dimension(sequence.size(), 2));
        for (int i = 0; i < sequence.size(); ++i) {
            vector[i, 0] = std::get<0>(sequence[i]);
            vector[i, 1] = std::get<1>(sequence[i]);
        }
        moves[person_idx] = vector;
    }
    movement_list[CharacterVector::create("moves")] = moves;
}


/*!
 * When you ask for movements for a particular human using R, it translates
 * the sample trajectory into an R list for easier processing.
 *
 * @param movement_list
 * @param human
 * @return
 */
// [[Rcpp::export]]
NumericVector movements_of_human(List movement_list, IntegerVector human) {
    auto result = as<XPtr<const movement_machine_result>>(movement_list["handle"]);
    auto sequence = result->movements_of_human(human[0]);
    auto vector = NumericVector(Dimension(sequence.size(), 2));
    for (int i=0; i < sequence.size(); ++i) {
        vector[i, 0] = std::get<0>(sequence[i]);
        vector[i, 1] = std::get<1>(sequence[i]);
    }
    return vector;
}


/*!
 * We need a method that takes R simulation of movement and converts
 * it into a *subclass of* a C++ movement_machine_result.
 */
