import { iso_today, generateUUID } from '../../support/helpers.js'

describe('Loading directly', function () {
    context('the show day page', function () {
        it('shows the page', function () {
            cy.add_exercise_to_storage('Exercise name 1', 10, 20, iso_today)
            cy.visit('http://localhost:8000/')
            cy.visit('http://localhost:8000/day/' + iso_today)
            cy.contains('Exercise name 1')
        })
    })

    context('the create exercise page', function () {
        it('shows the page', function () {
            cy.visit('http://localhost:8000/exercises/create')
            cy.contains('Name').next('input')
            cy.contains('Sets number').next('input')
            cy.contains('Repetitions number').next('input')
            cy.contains('Date').next('input')
        })
    })

    context('the edit exercise page', function () {
        it('shows the page with the values of the exercise', function () {
            var id = generateUUID()
            cy.add_exercise_to_storage('Exercise name 1', 10, 20, iso_today, id)
            cy.visit('http://localhost:8000/exercises/edit/' + id)
            cy.contains('Name').next('input').should('have.value', 'Exercise name 1')
            cy.contains('Sets number').next('input').should('have.value', '10')
            cy.contains('Repetitions number').next('input').should('have.value', '20')
            cy.contains('Date').next('input').should('have.value', iso_today)
        })
    })

    context('the delete exercise page', function () {
        it('deletes the exercise', function () {
            var id = generateUUID()
            cy.add_exercise_to_storage('Exercise name 1', 10, 20, iso_today, id)
            cy.visit('http://localhost:8000/exercises/delete/' + id)
            cy.location().should((location) => {
                expect(location.pathname).to.eq('/')
            })
            cy.get('.dayLink')
                .should('have.length', 0)
        })
    })

    // TODO: last exercises page
})
