import { iso_today, iso_tomorrow } from '../../support/helpers.js'

describe('The edit exercise page', function () {

    this.beforeEach(function () {
        cy.visit('http://localhost:8000/')
        cy.create_exercise('Exercise name', 10, 20, iso_today)
        cy.get('.dayLink').click()
        cy.get('.exercise').long_press()
        cy.get('.action-bar').contains('edit').click({ force: true })
    })

    it('allows the change the name of an exercise', function () {
        // Given a created exercise

        // The exercise is created in beforeEach

        // When the user edits the name of an exercise
        cy.contains('Name').next('input').clear().type('Edited name')
        cy.contains('button', 'Update').click()

        // Then the name of the exercise is changed
        cy.get('.exercise').contains('Edited name')
    })

    it('allows the change sets number of an exercise', function () {
        // Given a created exercise

        // The exercise is created in beforeEach

        // When the user edits the sets number of an exercise
        cy.contains('Sets number').next('input').clear().type('11')
        cy.contains('button', 'Update').click()

        // Then the sets number of the exercise is changed
        cy.get('.exercise').contains('11 sets')
    })

    it('allows the change repetitions number of an exercise', function () {
        // Given a created exercise

        // The exercise is created in beforeEach

        // When the user edits the repetitions number of an exercise
        cy.contains('Repetitions number').next('input').clear().type('21')
        cy.contains('button', 'Update').click()

        // Then the repetitions number of the exercise is changed
        cy.get('.exercise').contains('21 repetitions')
    })

    // TODO: allows the change the date of an exercise
    // TODO: do not change the exercise properties when cancelling the edit
    // TODO: contains the correct properties of the exercise in the fields
})
