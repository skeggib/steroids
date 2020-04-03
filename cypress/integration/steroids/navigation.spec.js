import { iso_today, iso_tomorrow } from '../../support/helpers.js'

describe('Cancelling an exercise creation', function () {
    it('goes back to the previous page', function () {
        it('from next days page', () => {
            cy.visit('http://localhost:8000/')
            cy.navigate_to_create_exercise_page()
            cy.contains('Cancel').click()
            cy.location().should((location) => {
                expect(location.pathname).to.eq('/')
            })
        })
        it('from past days page', () => {
            cy.visit('http://localhost:8000/past')
            cy.visit('http://localhost:8000/exercises/create')
            cy.contains('Cancel').click()
            cy.location().should((location) => {
                expect(location.pathname).to.eq('/')
            })
        })
    })
})

describe('Cancelling an exercise edit', function () {
    it('goes back to the day containing the exercise', function () {
        cy.add_exercise_to_storage('Exercise name 1', 10, 20, iso_today)
        cy.add_exercise_to_storage('Exercise name 2', 30, 40, iso_tomorrow)
        cy.visit('http://localhost:8000/')
        cy.get('.dayLink').first().click()
        cy.get('.exercise').long_press()
        cy.get('.action-bar').contains('edit').click({ force: true })
        cy.contains('Cancel').click()
        cy.contains('.exercise', 'Exercise name 1')
    })
})

describe('Editing the name of an exercise', function () {
    it('goes back to the day containing the exercise', function () {
        cy.add_exercise_to_storage('Exercise name 1', 10, 20, iso_today)
        cy.add_exercise_to_storage('Exercise name 2', 30, 40, iso_tomorrow)
        cy.visit('http://localhost:8000/')
        cy.get('.dayLink').first().click()
        cy.get('.exercise').long_press()
        cy.get('.action-bar').contains('edit').click({ force: true })
        cy.contains('Name').next('input').clear().type('Edited exercise name 1')
        cy.contains('Update').click()
        cy.contains('.exercise', 'Edited exercise name 1')
    })
})

describe('Editing the date of an exercise', function () {
    it('goes back to the new day containing the exercise', function () {
        cy.add_exercise_to_storage('Exercise name 1', 10, 20, iso_today)
        cy.add_exercise_to_storage('Exercise name 2', 30, 40, iso_tomorrow)
        cy.visit('http://localhost:8000/')
        cy.get('.dayLink').first().click()
        cy.get('.exercise').long_press()
        cy.get('.action-bar').contains('edit').click({ force: true })
        cy.contains('Date').next('input').clear().type(iso_tomorrow)
        cy.contains('Update').click()
        cy.contains('.exercise', 'Exercise name 1')
    })
})

describe('Deleting an exercise', function () {
    context('in a day containing multiple exercises', function () {
        this.beforeEach(() => {
            cy.add_exercise_to_storage('Exercise name 1', 10, 20, iso_today)
            cy.add_exercise_to_storage('Exercise name 2', 30, 40, iso_today)
            cy.visit('http://localhost:8000/')
        })

        it('stays on that day', function () {
            cy.get('.dayLink').first().click()
            cy.contains('Exercise name 1').long_press()
            cy.get('.action-bar').contains('delete').click({ force: true })
            cy.contains('.exercise', 'Exercise name 2')
        })

        it('hides the action bar', function () {
            cy.get('.dayLink').first().click()
            cy.contains('Exercise name 1').long_press()
            cy.get('.action-bar').contains('delete').click({ force: true })
            cy.get('.action-bar').should('not.be.visible')
        })
    })

    context('in a day containing only one exercise', function () {
        this.beforeEach(() => {
            cy.add_exercise_to_storage('Exercise name 1', 10, 20, iso_today)
            cy.add_exercise_to_storage('Exercise name 2', 30, 40, iso_tomorrow)
            cy.visit('http://localhost:8000/')
        })

        it('goes back to the main page', function () {
            cy.get('.dayLink').first().click()
            cy.get('.exercise').long_press()
            cy.get('.action-bar').contains('delete').click({ force: true })
            cy.location().should((location) => {
                expect(location.pathname).to.eq('/')
            })
        })
    })
})
