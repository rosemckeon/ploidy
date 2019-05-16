<p>77975878339</p>
<h1 id="how-does-disturbance-on-a-landscape-affect-the-establishment-of-new-polyploid-plant-species">How does disturbance on a landscape affect the establishment of new polyploid plant species?</h1>
<h2 id="consumables">Consumables?</h2>
<h3 id="software">Software</h3>
<p><a href="https://www.adobe.com/uk/creativecloud/plans.html?single_app=illustrator&amp;promoid=KSPCS&amp;mv=other">Creative Cloud</a></p>
<ul>
<li>All apps - 1yr (Student/Teacher discount) - £196.30 (incl. VAT)</li>
<li>Illustrator - 1yr (Schools/Universities pricing) - £151.68 (excl. VAT)</li>
</ul>
<blockquote>
<p>It would also need cancelling - I think it might try and roll over yearly automatically.</p>
</blockquote>
<h3 id="skills-development">Skills development:</h3>
<ul>
<li>£500 - 2 days (flexible), Aberdeen <a href="https://www.rgu.ac.uk/study/courses/793-practical-data-science-using-r">practical-data-science-using-r</a></li>
</ul>
<h3 id="conferences">Conferences:</h3>
<ul>
<li>275 euros - June 11th-14th, Ghent, Belgium <a href="https://vibconferences.be/event/international-conference-on-polyploidy">international-conference-on-polyploidy</a></li>
<li>435 euros - September 29th - 1st Oct, Sitges, Spain <a href="https://www.elsevier.com/events/conferences/plant-genome-evolution/about">plant-genome-evolution</a></li>
</ul>
<h3 id="free-courses">Free courses:</h3>
<ul>
<li>30th May, Edinburgh <a href="https://ukdataservice.ac.uk/news-and-events/eventsitem/?id=5479">Workshop: Understanding census microdata</a></li>
<li>17th May, Edinburgh <a href="https://cls.ucl.ac.uk/events/longitudinal-data-across-the-life-course-an-introduction-to-using-cohort-data/">longitudinal-cohort-data</a></li>
</ul>
<h3 id="what-i-said-id-do-in-my-proposal">What I said I’d do in my proposal:</h3>
<p>Simulated plants will be initialised on a spatially explicit landscape. Their genomes will be modelled so that, over generations, both auto and allopolyploidy can occur mechanistically – in line with some appropriate probability. Fitness costs (reproductive disadvantages) and benefits (increased stature and deleterious mutation buffering etc) of polyploidy will be parameterised to ensure a competitive element to the simulation. Plant diversification and polyploid distribution will be quantified through repeated simulations of the model across varying levels of disturbance (where the highest level causes complete mass plant death in a given area). This will test how important disturbance really is for predicting successful polyploid establishment and distribution.</p>
<h3 id="general-advice-from-greta">General advice from Greta:</h3>
<ul>
<li>Start very simple and add complexity gradually.</li>
<li>Keep dispersal ability the same for diploids and polyploids, (simple at first just like a king) so focus is on gigas-effects.</li>
<li>Test the gigas-effects one by one.</li>
<li>Test masking of deleterious mutation and adaptation to niches separately.</li>
<li>No need to populate landscape from the bottom as not looking at ranges.</li>
<li>No need to worry about spatial resolution.</li>
</ul>
<blockquote>
<p>If landscape cells are numbered 1-10 for habitat, and genes (also numbered 1-10) code for adaptation, then polyploids will have access to more matching genes that could be made use of (this may be too simplistic? as polyploids can also have novel adaptations - see genetics below - would allowing the polyploids to perform maths on the homologous genes be suffice to simulate this? so, for example of if they can add them up to match the landscape?). Also start simpler with binary habitats - add more if needed.</p>
</blockquote>
<h4 id="spatial-resolution">Spatial resolution:</h4>
<p>As it’s not based on a real geographical location, resolution will not bias my dispersal ranges or population growth etc. There is also no need to use a dispersal kernel for this model (So, I can have my landscape in squares, and my dispersal also in squares), though there are kernel models for pollen and seeds that could be used if wanted.</p>
<h2 id="questions-remaining">Questions remaining</h2>
<h3 id="can-i-make-the-improved-growth-rateother-gigas-effects-have-a-genetic-basis">Can I make the improved growth rate/other gigas effects have a genetic basis?</h3>
<p>My thoughts are that I would like the genetic complexity to be fairly detailed as it’s for the genetics society. This will make the evolution and spread of polyploid lineages through generations more accurate (because of the adaptive impact the genetics has). And it will give a number of mechanisms that could be tested separately to see which have the largest impact on results (I’m wondering if we can turn on and off things like gene conversion/biased expression/genome downsizing/etc).</p>
<p>My current understanding of how traits are modelled is that each allele will be represented numerically, resulting (for diploids) in a pair of figures which can be summed, or 1 used over the other if there is dominance. So, by having a larger pool of allele numbers to choose from the polyploid traits can become more powerful and flexible. From a pool of 4 random numbers between 1 and 10 the sum, if used for a trait, will likely be larger than the sum of 2 random numbers between 1 and 10. Or, if a match is required in the case of an adaptation, there is a larger pool of possibilities to make the match from.</p>
<h3 id="do-gigas-effects-affect-dispersal-ability">Do gigas-effects affect dispersal ability?</h3>
<p>Perhaps by:</p>
<ul>
<li>increasing seed wing size to bias wind dispersal range?</li>
<li>fruit size to bias frugivore dispersal?</li>
<li>inflorescence size to bias pollinators?</li>
</ul>
<p><strong>Should a genetic basis for these traits be included?</strong></p>
<h2 id="points-from-dinner">Points from dinner</h2>
<p>Brad talked about some kind of infinite allele model that we will use.</p>
<blockquote>
<p>“Paul Joyce and the infinite alleles model” added to Mendeley (READ).</p>
</blockquote>
<h2 id="genetically-explicit-details">Genetically Explicit Details</h2>
<p><a href="https://www.youtube.com/watch?v=JVro1y24IBA">See Wendell’s talk</a>, also full notes from talk in <a href="https://github.com/rozeykex/ploidy/blob/rose/genetics.md">genetics.md</a>.</p>
<h3 id="basic-cycle-according-to-wendell">Basic Cycle according to Wendell</h3>
<ol>
<li><strong>Whole genome doubling.</strong></li>
<li><strong>Biased expression:</strong>
<ul>
<li>Gene expression tends to bias to one of the two parents via gene conversion and gene loss.</li>
<li>Gene conversion: Neofunctionalization/Subfunctionalization/Gene transfer – Crosstalk between genomes that fully/partially converts homologous genes. This mechanism can delete deleterious mutation as well as create brand new adaptation/alleles that neither progenitor possesed.</li>
<li>Gene loss: One genome usually experiences more gene loss than the other. Difference in size between progenitor genomes can set direction of bias (in which direction? Perhaps type of environment encountered may also bias this?).</li>
</ul>
</li>
<li><strong>Biased fractionation:</strong>
<ul>
<li>Different kinds of genes are more likely to survive polyploid events, eg: transcription factors are retained (I’m guessing this is probably not something we’ll include).</li>
</ul>
</li>
<li><strong>Massive genome downsizing:</strong>
<ul>
<li>Continued gene loss over generations via chromosome rearrangement and number reduction.</li>
<li>Sudden burst of gene loss 10x that of diploids that reduces gradually (until when? diploid again?).</li>
<li>Gene loss in one genome stimulates biased expression and growth of the other.</li>
</ul>
</li>
<li>**Some time later, repeat. **</li>
</ol>
<blockquote>
<p><strong>Which bits of this are important?</strong><br>
I think the biased expression and genome downsizing should be included.</p>
</blockquote>
<p>Highlighted articles on neo and subfunctionalization from Wendell’s talk:</p>
<p><a href="https://bmcplantbiol-biomedcentral-com.ezproxy.stir.ac.uk/articles/10.1186/s12870-015-0511-8">Temporal transcriptome profiling reveals expression partitioning of homeologous genes contributing to heat and drought acclimation in wheat</a></p>
<p><a href="https://www-pnas-org.ezproxy.stir.ac.uk/content/108/38/16122">Extensive changes to alternative splicing patterns following allopolyploidy in natural and resynthesized polyploids</a></p>
<p><a href="https://genome-cshlp-org.ezproxy.stir.ac.uk/content/24/8/1348">The impact of widespread regulatory neofunctionalization on homeolog gene evolution following whole-genome duplication in maize</a></p>

