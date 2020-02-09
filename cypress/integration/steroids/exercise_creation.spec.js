describe('Exercise creation', function () {

    const today = new Date()
    const zeroPad = (num, places) => String(num).padStart(places, '0')
    const iso_today =
        zeroPad(today.getFullYear(), 4) +
        '-' +
        zeroPad(today.getMonth() + 1, 2) +
        '-' +
        zeroPad(today.getDate(), 2)
    const iso_tomorrow =
        zeroPad(today.getFullYear(), 4) +
        '-' +
        zeroPad(today.getMonth() + 1, 2) +
        '-' +
        zeroPad(today.getDate() + 1, 2)

    this.beforeEach(function () {
        cy.visit('http://localhost:8000/')
        cy.contains('Create an exercise').click()
    })

    it('Has the correct path', function () {
        cy.location().should((location) => {
            expect(location.pathname).to.eq('/exercises/create')
        })
    })

    it('Contains a name field', function () {
        cy.contains('Name').next('input')
    })

    it('Contains a sets number field', function () {
        cy.contains('Sets number').next('input')
    })

    it('Contains a repetitions number field', function () {
        cy.contains('Repetitions number').next('input')
    })

    it('Contains a date field', function () {
        cy.contains('Date').next('input')
    })

    it('Creates an exercise', function () {
        // Given some valids values entered in the input fields
        cy.contains('Name').next('input').type('Exercise name')
        cy.contains('Sets number').next('input').type('10')
        cy.contains('Repetitions number').next('input').type('20')
        cy.contains('Date').next('input').type(iso_today)

        // When the user clicks on the creation button
        cy.contains('button', 'Create').click()

        // Then the list of next exercises is displayed and it contains one day, this day also contains one exercise
        cy.location().should((location) => {
            expect(location.pathname).to.eq('/')
        })
        cy.get('.dayLink')
            .should('have.length', 1)
            .contains('1 exercise')
    })

    it('Clears the input fiels after creating an exercise', function () {
        // Given a created exercise
        cy.contains('Name').next('input').type('Exercise name')
        cy.contains('Sets number').next('input').type('10')
        cy.contains('Repetitions number').next('input').type('20')
        cy.contains('Date').next('input').type(iso_today)
        cy.contains('button', 'Create').click()

        // When the user wants to create another exercise
        cy.contains('Create an exercise').click()

        // Then the exercise creation page has empty input fields
        cy.contains('Name').next('input').should('have.value', '')
        cy.contains('Sets number').next('input').should('have.value', '')
        cy.contains('Repetitions number').next('input').should('have.value', '')
        cy.contains('Date').next('input').should('have.value', '')
    })

    it('Creates multiple exercises on the same day', function () {
        // Given a created exercise
        cy.contains('Name').next('input').type('Exercise name')
        cy.contains('Sets number').next('input').type('10')
        cy.contains('Repetitions number').next('input').type('20')
        cy.contains('Date').next('input').type(iso_today)
        cy.contains('button', 'Create').click()

        // When the user created another exercise
        cy.contains('Create an exercise').click()
        cy.contains('Name').next('input').type('Another exercise name')
        cy.contains('Sets number').next('input').type('30')
        cy.contains('Repetitions number').next('input').type('40')
        cy.contains('Date').next('input').type(iso_today)
        cy.contains('button', 'Create').click()

        // Then the list of next exercises is displayed and it contains one day, this day also contains two exercises
        cy.get('.dayLink')
            .should('have.length', 1)
            .contains('2 exercises')
    })

    it('Creates multiple exercises on different days', function () {
        // Given a created exercise
        cy.contains('Name').next('input').type('Exercise name')
        cy.contains('Sets number').next('input').type('10')
        cy.contains('Repetitions number').next('input').type('20')
        cy.contains('Date').next('input').type(iso_today)
        cy.contains('button', 'Create').click()

        // When the user created another exercise using a different date
        cy.contains('Create an exercise').click()
        cy.contains('Name').next('input').type('Another exercise name')
        cy.contains('Sets number').next('input').type('30')
        cy.contains('Repetitions number').next('input').type('40')
        cy.contains('Date').next('input').type(iso_tomorrow)
        cy.contains('button', 'Create').click()

        // Then the list of next exercises is displayed and it contains two days, each containing one exercise
        cy.get('.dayLink')
            .should('have.length', 2)
            .contains('1 exercise')
    })
})
