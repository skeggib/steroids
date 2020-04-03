import { iso_today, iso_tomorrow } from '../../support/helpers.js'

describe('Going back after cancelling an exercise creation', function () {
    it('does not go back to the exercise creation page', function () {
        cy.visit('http://localhost:8000/')
        cy.navigate_to_create_exercise_page()
        cy.contains('Cancel').click()
        cy.go('back')
        cy.location().should((location) => {
            expect(location.pathname).not.to.eq('/exercises/create')
        })
    })
})

describe('Going back after creating an exercise', function () {
    it('does not go back to the exercise creation page', function () {
        cy.visit('http://localhost:8000/')
        cy.navigate_to_create_exercise_page()
        cy.fill_input_fields('Exercise name', 10, 20, iso_today)
        cy.click_create_button()
        cy.go('back')
        cy.location().should((location) => {
            expect(location.pathname).not.to.eq('/exercises/create')
        })
    })
})

describe('Going back after cancelling an exercise edit', function () {
    it('does not go back to the exercise edition page', function () {
        cy.add_exercise_to_storage('Exercise name', 10, 20, iso_today)
        cy.visit('http://localhost:8000/')
        cy.get('.dayLink').click()
        cy.get('.exercise').long_press()
        cy.get('.action-bar').contains('edit').click({ force: true })
        cy.contains('Cancel').click()
        cy.go('back')
        cy.location().should((location) => {
            expect(location.pathname).not.to.contain('/exercises/edit')
        })
    })
})

describe('Going back after editing an exercise', function () {
    it('does not go back to the exercise edition page', function () {
        cy.add_exercise_to_storage('Exercise name', 10, 20, iso_today)
        cy.visit('http://localhost:8000/')
        cy.get('.dayLink').click()
        cy.get('.exercise').long_press()
        cy.get('.action-bar').contains('edit').click({ force: true })
        cy.contains('Update').click()
        cy.go('back')
        cy.location().should((location) => {
            expect(location.pathname).not.to.contain('/exercises/edit')
        })
    })
})

describe('Going back after deleting an exercise in a day containing other exercises', function () {
    it('goes back to the main page', function () {
        cy.add_exercise_to_storage('Exercise 2', 10, 20, iso_today)
        cy.add_exercise_to_storage('Exercise 1', 10, 20, iso_today)
        cy.visit('http://localhost:8000/')
        cy.get('.dayLink').click()
        cy.get('.exercise').first().long_press()
        cy.get('.action-bar').contains('delete').click({ force: true })
        cy.go('back')
        cy.location().should((location) => {
            expect(location.pathname).to.eq('/')
        })
    })
})
