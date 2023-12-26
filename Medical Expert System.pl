%To Start the system type start.
% Name : - Fahmin Guliyev.

:- use_module(library(jpl)).
start :-sleep(0.4),	
		write('-----------------------------------------------------------------'),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.2),
		write("###################||| EXPERT SYSTEM |||#########################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write('-----------------------------------------------------------------'),nl,nl,nl,
		
		
        /*write("Hi. How are you? First of all tell me your name Please : "),
        read(Patient),*/
		
		
		interface2.
		
		
       /* hypothesis(Patient,Disease),
        write(Patient),write(', you '), write(' probably have '),write(Disease),write('.'),undo,
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| THANK YOU FOR USING OUR PROGRAM |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.*/
        
        
    symptom(Patient,fever) :- verify(Patient," have a fever (y/n) ?").
 
    symptom(Patient,sores_or_blisters) :- verify(Patient," have sores or blisters on the face and on the body such as the stomach chest or back (y/n) ?").
  
    symptom(Patient,chew_things) :- verify(Patient," an infant and is the infant drooling or wanting to chew on things  (y/n) ?").

    symptom(Patient,red_swollen_gums) :- verify(Patient," have red and swollen gums that may bleed when he or she brushes or flosses (y/n) ?").
    
    symptom(Patient,honey_crusting) :- verify(Patient," have honey colored crusting on the mouth that began as an itchy red sore or cluster of blisters (y/n) ?").
    
    symptom(Patient,red_purple_sore) :- verify(Patient," have a red or purple sore or cluster of sores on a lip or the outer edge of the lips (y/n) ?").
	
    symptom(Patient,yellowish_sores) :- verify(Patient," have small open and painful sores that are white yellowish with a red border on the inner lips (y/n) ?").
 
    symptom(Patient,bumps_tongue) :- verify(Patient," have small painful bumps on the tongue (y/n) ?").
   
    symptom(Patient,bluish_sac) :- verify(Patient," have a small painless fluid filled sac that may be bluish in color on the inner lips gums palate (y/n) ?").
  
    symptom(Patient,creamy_patches) :- verify(Patient," have creamy white patches on the tongue inner cheek or gums and are they painful when scraped (y/n) ?").
   
    symptom(Patient,swollen_tongue) :- verify(Patient," have a sore or swollen tongue or lips (y/n) ?").

    symptom(Patient,sore_throat) :- verify(Patient," have a sore throat and painful blisters on the tongue or mouth or a rash on the palms of hands (y/n) ?").

    symptom(Patient,redness_cheeks) :- verify(Patient," have painful sores swelling or redness on the inside of the cheeks and on the gums (y/n) ?").

	symptom(Patient,white_patches) :- verify(Patient," have a red and sore throat with white patches on the throat or tonsils").
	
	/*symptom(_,"Sorry, the program could not be able to diagnose the disease.").*/

        
    hypothesis(Patient,teething) :-
        symptom(Patient,chew_things).
    
    hypothesis(Patient,gingivitis) :-
        symptom(Patient,red_swollen_gums).
        
    hypothesis(Patient,impetigo) :-
        symptom(Patient,honey_crusting).

	hypothesis(Patient,cold_sore) :-
		symptom(Patient,red_purple_sore).

	hypothesis(Patient,canker_sores) :-
		symptom(Patient,yellowish_sores).

    hypothesis(Patient,inflamed_papillae) :-
		symptom(Patient,bumps_tongue).
        
    hypothesis(Patient,mucocele) :-
		symptom(Patient,bluish_sac).
    
    hypothesis(Patient,oral_thrush) :-
		symptom(Patient,creamy_patches).
    
    hypothesis(Patient,allergic_reaction) :-
		symptom(Patient,swollen_tongue).

	hypothesis(Patient,chickenpox) :-
		symptom(Patient,fever),
		symptom(Patient,sores_or_blisters).

	hypothesis(Patient,hand_foot_mouth) :-
		symptom(Patient,sore_throat).

	hypothesis(Patient,strep_throats) :-
		symptom(Patient,white_patches).

	hypothesis(Patient,gingivostomatitis) :-
		symptom(Patient,redness_cheeks).
        
	hypothesis(_,"disease. Sorry, the program could not be able to diagnose the disease.").
	
    response(Reply) :-
        read(Reply),
        write(Reply),nl.
		
ask(Patient,Question) :-
	write(Patient),write(', does your child'),write(Question),
	/*read(N),
	( (N == yes ; N == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail),*/
	
	interface(', does your child',Patient,Question),
	write('Loading.'),nl,
	sleep(1),
	write('Loading..'),nl,
	sleep(1),
	write('Loading...'),nl,
	sleep(1),
    nl.
	
:- dynamic yes/1,no/1.		
	
verify(P,S) :-
   (yes(S) 
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(P,S))).
	 
undo :- retract(yes(_)),fail. 
undo :- retract(no(_)),fail.
undo.


pt(Patient):- 

		hypothesis(Patient,Disease),
		interface3(Patient,', you probably have some',Disease,'.'),
        write(Patient),write(', you probably have some'),write(Disease),write('.'),undo,end.

end :-
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| THANK YOU FOR USING OUR PROGRAM |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.

interface(X,Y,Z) :-
	atom_concat(Y,X, FAtom),
	atom_concat(FAtom,Z,FinalAtom),
	jpl_new('javax.swing.JFrame', ['Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- MEDICAL EXPERT SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,FinalAtom], N),
	jpl_call(F, dispose, [], _), 
	write(N),nl,
	( (N == yes ; N == y)
      ->
       assert(yes(Z)) ;
       assert(no(Z)), fail).
	   		
interface2 :-
	jpl_new('javax.swing.JFrame', ['Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- MEDICAL EXPERT SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Hi. How are you? First of all tell me your name please'], N),
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(null)
		->	write('you cancelled'),interface3('you cancelled. ','Thank you ','for using ','the program.'),end,fail
		;	write("Hi. How are you? First of all tell me your name please : "),write(N),nl,pt(N)
	).
	
	
interface3(P,W1,D,W2) :-
	atom_concat(P,W1, A),
	atom_concat(A,D,B),
	atom_concat(B,W2,W3),
	jpl_new('javax.swing.JFrame', ['Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- MEDICAL EXPERT SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,W3], N),
	jpl_call(F, dispose, [], _),
	/*write(N),nl,*/
	(	N == @(void)
		->	write('')
		;	write("")
	).
	
help :- write("To start the expert system please type 'start.' and press the Enter key").