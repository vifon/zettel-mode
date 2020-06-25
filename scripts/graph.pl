#!/usr/bin/env perl

=head1 zettel-graph.pl

Generate the input for Graphviz C<dot(1)> to generate the graph of notes
in the current directory and the links between them.

=head2 USAGE

  cd ~/.deft
  .../graph.pl | dot -Tx11

or

  .../graph.pl | dot -Tpdf -o graph.pdf

=cut

use warnings;
use strict;
use 5.010;
use autodie;

sub title (_) {
    my $filename = shift;

    my $title;
    open(my $f, '<', $filename);
    while (<$f>) {
        $title = $1 and last if /^#\+TITLE:\s*(.*)/
    }
    close $f;
    $_ = $title // $filename =~ s/\.org$//r;
}

say 'digraph org {';
for my $file (<*.org>) {
    my $file_title = title $file;

    my @links = `grep -l -F "[file:$file]" *.org`;
    print '  {';
    print join " ", map {chomp; title; qq("$_")} @links;
    say qq(} -> "$file_title");
    say qq(  "$file_title" [URL="$file"]);
}
say '}';
