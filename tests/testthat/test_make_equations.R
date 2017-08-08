context("solong")

test_that("equation constructor works", {

    myref <- bibentry(bibtype="Article",key="Ref0000",
                      author=c(person("A","Person")),
                      year=2000,
                      title="Marvellous study of stuff",
                      journal="Journal of Great Importance",
                      volume=42,pages="1-10",doi="10.0000/123456789")

    ## complete and working examples
    eq <- sol_make_equation(equation_id="myequation001",
                            taxon_name="thingy thingy",
                            taxon_aphia_id=0,
                            equation=function(SL)tibble(allometric_value=2*SL),
                            inputs=tibble(property="standard length",units="mm",sample_minimum=6,sample_maximum=16),
                            return_property="wet weight",
                            return_units="kg",
                            reliability=tibble(type="N",value="100"),
                            reference=myref)

    ## missing "recommended" stuff should throw warnings
    expect_warning(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=myref),
        "no taxon_aphia_id")

    expect_warning(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="kg",
                                reference=myref),
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
                                reference=myref),
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
                                reference=myref),
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
                                reference=myref),
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
                                reference=myref),
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
                                reference=myref),
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
                                reference=myref),
        "input units .* are not compatible")
    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="bilbobaggins",units="mm"), ## invalid property
                                return_property="wet weight",
                                return_units="kg",
                                reliability=tibble(type="N",value="100"),
                                reference=myref),
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
                                reference=myref),
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
                                reference=myref),
        "return_units .* are not compatible")

    expect_error(
        eq <- sol_make_equation(equation_id="myequation001",
                                taxon_name="thingy thingy",
                                taxon_aphia_id=0,
                                equation=function(...)tibble(allometric_value=2*...),
                                inputs=tibble(property="standard length",units="mm"),
                                return_property="wet weight",
                                return_units="m",
                                reliability=tibble(type="N",value="100"),
                                reference=myref),
        "return_units .* are not compatible with the return_property")
})
