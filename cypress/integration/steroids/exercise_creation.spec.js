import { iso_today, iso_tomorrow } from '../../support/helpers.js'

describe('The exercise creation page', function () {

    /**
     * Get the list of input fields (<input> tags) of the exercise creation page.
     */
    function get_input_fields_list() {
        return [
            cy.contains('Name').next('input'),
            cy.contains('Sets number').next('input'),
            cy.contains('Repetitions number').next('input'),
            cy.contains('Date').next('input')
        ]
    }

    this.beforeEach(function () {
        cy.visit('http://localhost:8000/')
        cy.navigate_to_create_exercise_page()
    })

    it('has the correct path', function () {
        cy.location().should((location) => {
            expect(location.pathname).to.eq('/exercises/create')
        })
    })

    it('contains a name field', function () {
        cy.contains('Name').next('input')
    })

    it('contains a sets number field', function () {
        cy.contains('Sets number').next('input')
    })

    it('contains a repetitions number field', function () {
        cy.contains('Repetitions number').next('input')
    })

    it('contains a date field', function () {
        cy.contains('Date').next('input')
    })

    it('allows to creates an exercise', function () {
        // Given some valids values entered in the input fields
        cy.fill_input_fields('Exercise name', 10, 20, iso_today)

        // When the user clicks on the creation button
        cy.click_create_button()

        // Then the list of next exercises is displayed and it contains one day, this day also contains one exercise
        cy.location().should((location) => {
            expect(location.pathname).to.eq('/') // TODO: move to another test
        })
        cy.get('.dayLink')
            .should('have.length', 1)
            .contains('1 exercise')
    })

    it('clears the input fiels after creating an exercise', function () {
        // Given a created exercise
        cy.fill_input_fields('Exercise name', 10, 20, iso_today)
        cy.click_create_button()

        // When the user wants to create another exercise
        cy.contains('Create an exercise').click()

        // Then the exercise creation page has empty input fields
        get_input_fields_list().forEach(input => input.should('have.value', ''))
    })

    // TODO: it does not clear the input fields when the creation was cancelled
    // TODO: it creates an exercise using the values entered in the fields
    // TODO: it displays error when some fields are incorrect
})
