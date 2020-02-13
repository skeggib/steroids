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
        cy.fill_input_fields('Exercise name', 10, 20, iso_today)

        // When the user clicks on the creation button
        cy.click_create_button()

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
        cy.fill_input_fields('Exercise name', 10, 20, iso_today)
        cy.click_create_button()

        // When the user wants to create another exercise
        cy.contains('Create an exercise').click()

        // Then the exercise creation page has empty input fields
        get_input_fields_list().forEach(input => input.should('have.value', ''))
    })

    it('Creates multiple exercises on the same day', function () {
        // Given a created exercise
        cy.fill_input_fields('Exercise name', 10, 20, iso_today)
        cy.click_create_button()

        // When the user created another exercise
        cy.navigate_to_create_exercise_page()
        cy.fill_input_fields('Another exercise name', 30, 40, iso_today)
        cy.click_create_button()

        // Then the list of next exercises is displayed and it contains one day, this day also contains two exercises
        cy.get('.dayLink')
            .should('have.length', 1)
            .contains('2 exercises')
    })

    it('Creates multiple exercises on different days', function () {
        // Given a created exercise
        cy.fill_input_fields('Exercise name', 10, 20, iso_today)
        cy.click_create_button()

        // When the user created another exercise using a different date
        cy.navigate_to_create_exercise_page()
        cy.fill_input_fields('Another exercise name', 30, 40, iso_tomorrow)
        cy.click_create_button()

        // Then the list of next exercises is displayed and it contains two days, each containing one exercise
        cy.get('.dayLink')
            .should('have.length', 2)
            .contains('1 exercise')
    })

    // TODO: it does not clear the input fields when the creation was cancelled
    // TODO: it creates an exercise using the values entered in the fields
    // TODO: it displays error when some fields are incorrect
})
