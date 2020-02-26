import { iso_today } from '../../support/helpers.js'

describe('The edit exercise page', function () {

    this.beforeEach(function () {
        cy.add_exercise_to_storage('Exercise name', 10, 20, iso_today)
        cy.visit('http://localhost:8000/')
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

    it('keeps the changes after a page reload', function () {
        cy.contains('Name').next('input').clear().type('Edited name')
        cy.contains('Sets number').next('input').clear().type('11')
        cy.contains('Repetitions number').next('input').clear().type('21')
        cy.contains('button', 'Update').click()

        cy.visit('http://localhost:8000/day/' + iso_today)

        cy.get('.exercise').contains('Edited name')
        cy.get('.exercise').contains('11 sets')
        cy.get('.exercise').contains('21 repetitions')
    })

    // TODO: allows the change the date of an exercise
    // TODO: do not change the exercise properties when cancelling the edit
    // TODO: contains the correct properties of the exercise in the fields
})

describe('Clicking on the checkbox of an exercise', function () {
    this.beforeEach(function () {
        cy.add_exercise_to_storage('Exercise name', 10, 20, iso_today)
        cy.visit('http://localhost:8000/')
        cy.get('.dayLink').click()
    })

    context('when the checkbox is not checked', function () {
        this.beforeEach(function () {
            cy.get('.exercise').parent().contains('radio_button_unchecked').click()
        })

        it('checks the checkbox', function () {
            cy.get('.exercise').parent().contains('check_circle')
        })

        it('keeps the checkbox checked when going to another page and back', function () {
            cy.go('back')
            cy.get('.dayLink').click()
            cy.get('.exercise').parent().contains('check_circle')
        })

        it('keeps the checkbox checked when reloading the page', function () {
            cy.visit('http://localhost:8000/day/' + iso_today)
            cy.get('.exercise').parent().contains('check_circle')
        })
    })

    context('when the checkbox is already checked', function () {
        this.beforeEach(function () {
            cy.get('.exercise').parent().contains('radio_button_unchecked').click()
            cy.get('.exercise').parent().contains('check_circle')
            cy.get('.exercise').parent().contains('check_circle').click()
        })

        it('unchecks the checkbox', function () {
            cy.get('.exercise').parent().contains('radio_button_unchecked')
        })
    })
})
