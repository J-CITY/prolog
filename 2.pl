sentence               --> noun_phrase, verb_phrase, separator_sentence. % A sentence consist of a noun phrase, a verb phrase and a ".".

noun_phrase            --> pronoun_1person.                              % A noun phrase can be the pronoun "mi"      or
noun_phrase            --> pronoun_2person.                              % the pronoun "sina"                         or
noun_phrase            --> noun, separator_noun_phrase.                  % a noun plus "li".

verb_phrase            --> verb_transitive.                              % A verb phrase can be a transitive verb     or  
verb_phrase            --> noun.                                         % a noun. In toki pona is no verb "to be".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Words 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

noun                   --> [jan].      % person, people, human, being, somebody, anybody
noun                   --> [moku].     % food, meal
noun                   --> [pona].     % good, simplicity, positivity
noun                   --> [suli].     % size
noun                   --> [suno].     % sun, light
noun                   --> [telo].     % water, liquid, juice, sauce

pronoun_1person        --> [mi].       % I, we
pronoun_2person        --> [sina].     % you

separator_sentence     --> ['.'].      % At the end of a sentence.
separator_noun_phrase  --> [li].       % between any subject except mi and sina and its verb

verb_transitive        --> [jan].      % personify, humanize, personalize
verb_transitive        --> [moku].     % eat, drink, swallow, ingest, consume
verb_transitive        --> [pona].     % improve, fix, repair, make good
verb_transitive        --> [suli].     % enlarge, lengthen
verb_transitive        --> [telo].     % water, wash with water
