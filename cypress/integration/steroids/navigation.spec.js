import { iso_today, iso_tomorrow } from '../../support/helpers.js'

describe('Cancelling an exercise edit', function () {
    it('goes back to the day containing the exercise', function () {
        cy.visit('http://localhost:8000/')
        cy.create_exercise('Exercise name 1', 10, 20, iso_today)
        cy.create_exercise('Exercise name 2', 30, 40, iso_tomorrow)
        cy.get('.dayLink').first().click()
        cy.get('.exercise').long_press()
        cy.get('.action-bar').contains('edit').click({ force: true })
        cy.contains('Cancel').click()
        cy.contains('.exercise', 'Exercise name 1')
    })
})

describe('Editing the name of an exercise', function () {
    it('goes back to the day containing the exercise', function () {
        cy.visit('http://localhost:8000/')
        cy.create_exercise('Exercise name 1', 10, 20, iso_today)
        cy.create_exercise('Exercise name 2', 30, 40, iso_tomorrow)
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
        cy.visit('http://localhost:8000/')
        cy.create_exercise('Exercise name 1', 10, 20, iso_today)
        cy.create_exercise('Exercise name 2', 30, 40, iso_tomorrow)
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
            cy.visit('http://localhost:8000/')
            cy.create_exercise('Exercise name 1', 10, 20, iso_today)
            cy.create_exercise('Exercise name 2', 30, 40, iso_today)
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
            cy.visit('http://localhost:8000/')
            cy.create_exercise('Exercise name 1', 10, 20, iso_today)
            cy.create_exercise('Exercise name 2', 30, 40, iso_tomorrow)
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
