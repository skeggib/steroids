import { iso_today } from '../../support/helpers.js'

describe('The action bar', function () {

    this.beforeEach(function () {
        cy.visit('http://localhost:8000/')
        cy.create_exercise('Exercise name', 10, 20, iso_today)
        cy.get('.dayLink').click()
        cy.get('.exercise').long_press()
    })

    it('allows to delete an exercise', function () {
        cy.get('.action-bar').contains('delete').click({ force: true })
        cy.location().should((location) => {
            expect(location.pathname).to.eq('/')
        })
        cy.get('.dayLink').should('have.length', 0)
    })
})
