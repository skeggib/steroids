import { iso_today, iso_tomorrow } from '../../support/helpers.js'

describe('The next days page', function () {

    this.beforeEach(function () {
        cy.visit('http://localhost:8000/')
    })

    it('displays multiple exercises on the same day in a single entry', function () {
        // Given a created exercise
        cy.navigate_to_create_exercise_page()
        cy.fill_input_fields('Exercise name', 10, 20, iso_today)
        cy.click_create_button()

        // When the user created another exercise
        cy.visit('http://localhost:8000/')
        cy.navigate_to_create_exercise_page()
        cy.fill_input_fields('Another exercise name', 30, 40, iso_today)
        cy.click_create_button()

        // Then the list of next exercises is displayed and it contains one day, this day also contains two exercises
        cy.visit('http://localhost:8000/')
        cy.get('.dayLink')
            .should('have.length', 1)
            .contains('2 exercises')
    })

    it('displays multiple exercises on different days in different entries', function () {
        // Given a created exercise
        cy.navigate_to_create_exercise_page()
        cy.fill_input_fields('Exercise name', 10, 20, iso_today)
        cy.click_create_button()

        // When the user created another exercise using a different date
        cy.visit('http://localhost:8000/')
        cy.navigate_to_create_exercise_page()
        cy.fill_input_fields('Another exercise name', 30, 40, iso_tomorrow)
        cy.click_create_button()

        // Then the list of next exercises is displayed and it contains two days, each containing one exercise
        cy.visit('http://localhost:8000/')
        cy.get('.dayLink')
            .should('have.length', 2)
            .contains('1 exercise')
    })
})
