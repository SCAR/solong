context("solong")

test_that("equation constructor works", {
    ## complete and working examples
    eq <- sol_make_equation(equation_id="myequation001",
                            taxon_name="thingy thingy",
                            taxon_aphia_id=0,
                            equation=function(SL)tibble(allometric_value=2*SL),
                            inputs=tibble(property="standard length",units="mm",sample_minimum=6,sample_maximum=16),
                            return_property="wet weight",
                            return_units="kg",
                            reliability=tibble(type="N",value="100"),
                            reference="")

    ## missing "recommended" stuff should throw warnings
    expect_warning(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "no taxon_aphia_id")

    expect_warning(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="kg",
                                reference=""),
        "no reliability information")

    expect_warning(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100")),
        "no reference")

    ## errors if mandatory stuff missing or incorrect
    expect_error(
        eq <- sol_make_equation(##equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "\"equation_id\" is missing")
    expect_error(
        eq <- sol_make_equation(equation_id=3,
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "equation_id is not a string")
    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                ##taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "\"taxon_name\" is missing")
    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs="standard length", ## invalid inputs
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "inputs is not a data frame")
    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(thing="standard length",units="mm"), ## invalid inputs
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "the inputs data.frame should have the columns \"property\" and \"units\"")
    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mmmm"), ## invalid units
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "input units .* are not recognized")
    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="bilbobaggins",units="mm"), ## invalid property
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "input property not recognized: \"bilbobaggins\"")

    ## return property and units
    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="blah",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "return property .* not recognized")
    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="frogs",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "return_units .* are not recognized")

    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="m",
                                reliability=tibble(type="N",value="100"),
                                reference=""),
        "return_units .* are not compatible with the return_property")
})
