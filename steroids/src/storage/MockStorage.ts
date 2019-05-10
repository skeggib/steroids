import IStorage from './IStorage';
import Session from '../model/Session';
import Exercice from '../model/Exercice';
import Serie from '../model/Serie';
import { Observable, Observer } from 'rxjs';
import { find, first, map } from 'rxjs/operators';

export default class MockStorage implements IStorage {

    private nextSessionId = 3;
    private nextExerciceId = 3;
    private nextSerieId = 1;

    private sessions: Session[] =
        [
            {
                id: 1,
                name: 'Pec',
                date: new Date(),
                exercices: [
                    {
                        id: 1,
                        name: 'Développé couché',
                        series: [{
                            id: 5,
                            repetition: 20,
                            weight: 30,
                            rating: 3
                        }],
                        comment: 'RAS'
                    }
                ]
            },
            {
                id: 2,
                name: 'Dos',
                date: new Date(),
                exercices: [
                    {
                        id: 2,
                        name: 'Poulie',
                        series: [],
                        comment: 'Ce n\'était pas aisé...'
                    }
                ]
            }
        ];

    getAllSessions(): Observable<Session[]> {
        return Observable.create((observer: Observer<Session[]>) => {
            observer.next(this.sessions);
            observer.complete();
        });
    }

    getSession(id: number): Observable<Session> {
        return this.getAllSessions().pipe(
            map(sessions => sessions.find(session => session.id === id)));
    }

    createSession(session: Session): Observable<void> {
        return Observable.create((observer: Observer<Session[]>) => {
            session.id = this.nextSessionId++;
            this.sessions.push(session);
            observer.complete();
        });
    }

    addExercice(exercice: Exercice, session: Session): Observable<void> {
        return Observable.create((observer: Observer<Session[]>) => {
            exercice.id = this.nextExerciceId++;
            this.getSession(session.id).subscribe(s =>
                s.exercices.push(exercice));
            observer.complete();
        });
    }

    getExercice(id: number): Observable<Exercice> {
        return Observable.create((observer: Observer<Exercice>) => {
            this.getAllSessions().subscribe((sessions: Session[]) => {
                sessions.forEach((session: Session) => {
                    const exercice = session.exercices.find(e => e.id === id);
                    if (exercice !== undefined) {
                        observer.next(exercice);
                        return;
                    }
                });
                observer.complete();
            });
        });
    }

    addSerie(serie: Serie, exercice: Exercice): Observable<void> {
        return Observable.create((observer: Observer<Session[]>) => {
            serie.id = this.nextSerieId++;
            this.getExercice(exercice.id).subscribe(exercice => {
                exercice.series.push(serie);
                observer.complete();
            });
        });
    }

    getAllExerciceTemplates(): Observable<Exercice[]> {
        return this.getAllSessions().pipe(
            map(sessions => sessions.map(session => session.exercices)
                .reduce((ex1, ex2) => ex1.concat(ex2))
                .map(exercice => new Exercice(exercice.name))));
    }
}
