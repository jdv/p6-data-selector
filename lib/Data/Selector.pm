use v6;

grammar Data::Selector::SelectorString::Grammar {
    token TOP {
        ^ <selector_group> $
    }

    token selector_group {
        '[' ~ ']' [ <selector>+ % ',' ]
    }

    token selector {
        [ <selector_path_part> | <selector_group> | <named_selector> ]+ % '.'
    }

    token selector_path_part {
        <-[[\].\$,]>+ [ '..' <-[[\].\$,]>+ ]?
    }

    token named_selector {
        \$<[a..z_]>+
    }
}

class Data::Selector::SelectorString::Actions {
    has $.order is rw;
    has $.named_selectors is rw;

    method TOP( Match $/ --> Hash ) {
        make $/.<selector_group>>>.ast.hash;
    }

    method selector_group( Match $/ --> Hash ) {
        my Hash %h;
        for @( $/.<selector> ) -> $v {
            for $v.ast.kv -> $k, $v {
                for $v.kv -> $k2, $v2 {
                    %h{$k}{$k2} = $v2;
                }
                my $first_char = $k.substr( 0, 1 );
                my $inverse_k =
                ( $first_char eq "-" ?? "+" !! "-" ) ~ $k.substr( 1 );
                %h{$inverse_k}:delete;
            }
        }
        make %h;
    }

    method selector( Match $/ --> Hash ) {
        my @h_current;
        @h_current[0] = my Hash %h_root;
        # TODO:  why need parens here?  used to work without.
        for $/.caps>>.kv -> ( $k, $v ) {
            if $k eq 'selector_path_part' {
                for $v.ast.kv -> $k is copy, $v {
                    unless $k.substr(0,1) eq '+' || $k.substr(0,1) eq '-' {
                        $k = "+$k"
                    }

                    for @h_current.keys -> $i {
                        @h_current[$i] = @h_current[$i]{$k} = Hash.new( %( $v ) );
                    }
                }
            }
            elsif $k eq 'selector_group' {
                my @h_cur_new;
                for $v.ast.kv -> $k, $v {
                    for @h_current.keys -> $i {
                        push( @h_cur_new, ( @h_current[$i]{$k} = Hash.new(%($v)) ) );
                    }
                }
                @h_current = @h_cur_new;
            }
            elsif $k eq 'named_selector' {
                my @h_cur_new;
                for $v.ast.kv -> $k, $v {
                    for @h_current.keys -> $i {
                        push( @h_cur_new, ( @h_current[$i]{$k} = Hash.new(%($v)) ) );
                    }
                }
                @h_current = @h_cur_new;
            }
        }
        make %h_root;
    }

    method selector_path_part( Match $/ --> Hash ) {
        make %( $/ => %( _order_ => ++$.order, ), );
    }

    method named_selector( Match $/ --> Hash ) {
        die "unknown named selector:  $/" unless $.named_selectors;
        my Str $selector_string = $.named_selectors{"$/"} ~ '';
        my $action_object = Data::Selector::SelectorString::Actions.new(
            order => $.order,
            named_selectors => $.named_selectors,
        );
        my Hash $ast;
        {
            my $/;
            $ast = Data::Selector::SelectorString::Grammar.parse(
                "[$selector_string]",
                :actions( $action_object ),
            ).ast;
        }
        $.order = $action_object.order;
        make $ast;
    }
}

class Data::Selector {
    method parse_string ( Str :$selector_string!, Hash :$named_selectors --> Hash ) {
        my $tree = Data::Selector::SelectorString::Grammar.parse(
            "[$selector_string]",
            actions => Data::Selector::SelectorString::Actions.new(
                named_selectors => $named_selectors
            )
        ).ast;

        sub reorder ( $tree ) {
            my $order;
            my @queue = ( $tree );
            while @queue.elems {
                my $t = @queue.shift;
                my @sorted_keys = $t.keys.grep(
                    { $_ ne '_order_' }
                ).sort(
                    { $t{$^a}<_order_> cmp $t{$^b}<_order_> }
                );
                for @sorted_keys -> $k {
                    $t{$k}<_order_> = ++$order;
                    @queue.push( $t{$k} );
                }
            }
            return $tree;
        }

        return reorder( $tree );
    }

    method apply_tree ( Hash :$selector_tree!, Hash :$data_tree! is rw --> Nil ) {
        my @queue = ( [ $selector_tree, $data_tree, ], );
        my %selector_trees_keys;
        while (@queue) {
            my ( $selector_tree, $data_tree, ) = @( shift @queue );

            %selector_trees_keys{$selector_tree.WHICH} ||= [
                $selector_tree.keys.grep(
                    { $_ ne '_order_'; }
                ).sort(
                    { $selector_tree{$^a}<_order_>
                      <=> $selector_tree{$^b}<_order_>; }
                ).map(
                    {
                        my $selector_tree_key_base = $_.substr( 1, );
                        [
                            $_,
                            $selector_tree_key_base,
                            Any,
                            Any,
                            Any,
                        ];
                    }
                );
            ];

            my @data_tree_keys =
            $data_tree ~~ Hash
            ?? keys %( $data_tree )
            !! ( 0..^$data_tree );

            my @selector_tree_keys = @( %selector_trees_keys{$selector_tree.WHICH} );
            my $has_includes;
            for (@selector_tree_keys) {
                $has_includes = 1
                if !$has_includes && $_[0].index( '+', ) === 0;
                if $_[0].index( '+-' ) === 0 || $_[0].index( '--' ) === 0 {
                    if $_[0] ~~ /^('+'|'-')('-'\d+)$/ {
                        $_[4] = $_[0];
                        if $1 < 0 && $1 >= -@($data_tree) {
                            $_[0] = $0 ~ (  @($data_tree) + $1 );
                        }
                        else {
                            $0 ~ substr( $1, 1, );
                        }
                        $_[1] = substr( $_[0], 1, );
                    }
                }

                if $data_tree ~~ Array && $_[0].index( '..' ).defined {
                    $_[0] ~~ /^['+'|'-']('-'?\d+)\.\.('-'?\d+)$/;
                    my @array_range = +$0, +$1;
                    $_[3]= @array_range.map: {
                        $_ < 0 ??  @($data_tree) + $_ !! $_;
                    };
                }
            }

            my %matching_selector_keys_by_data_key;
            my $data_tree_keys_string = join( "\n", @data_tree_keys, ) ~ "\n";
            for @selector_tree_keys {
                #TODO: not sure why [] required here but probably RT#121024
                my $selector_tree_key_pattern
                  = '[' ~ join( '\n|', $_[3][0] .. $_[3][1], ) ~ '\n]' if $_[3].defined;

                unless $selector_tree_key_pattern.defined {
                    $selector_tree_key_pattern = ( $_[2] || $_[1] ) ~ '\n';
                    #TODO: timtoady says already an RT maybe
                    my $/;
                    $selector_tree_key_pattern ~~ s:g/(\W)/\\$0/;
                    $selector_tree_key_pattern = $selector_tree_key_pattern.trans(
                        [ '\\*', '\\\\n', '\\-0\\\\n' ] => [ '\N*?', '\n', '0\n' ]
                    );
                }
                my @matches =
                $data_tree_keys_string ~~ m:global/<$selector_tree_key_pattern>/;
                for @matches -> $data_tree_key {
                    push(
                        %matching_selector_keys_by_data_key{$data_tree_key.chop},
                        $_[4] // $_[0],
                    );
                }
            }

            my ( %arrays_to_be_trimmed, %deferred_excludes, %matched_includes, );
            for keys %matching_selector_keys_by_data_key -> $data_tree_key {
                my $matching_selector_keys =
                %matching_selector_keys_by_data_key{$data_tree_key};
                if ( index( $matching_selector_keys[*-1], '-', ) === 0 ) {
                    if ( $data_tree ~~ Hash ) {
                        $data_tree{$data_tree_key}:delete;
                    }
                    else {
                        my $ok =
                        try { $data_tree[$data_tree_key] = '_to_be_trimmed_'; };
                        %arrays_to_be_trimmed{$data_tree.WHICH} = $data_tree if $ok;
                    }
                }
                else {
                    %matched_includes{$data_tree.WHICH}{$data_tree_key}++;
                    %deferred_excludes{$data_tree.WHICH}{$data_tree_key}:delete;

                    my $matched_includes_for_data_tree =
                    %matched_includes{$data_tree.WHICH};
                    my @data_keys_to_be_deferred = @data_tree_keys.grep: {
                        !$matched_includes_for_data_tree{$_};
                    };
                    %deferred_excludes{$data_tree.WHICH}{$_}
                    = $data_tree for @data_keys_to_be_deferred;

                    my $data_sub_tree =
                        $data_tree ~~ Hash
                    ?? $data_tree{$data_tree_key}
                    !! try { $data_tree[$data_tree_key] };

                    my $selector_sub_tree =
                    @($matching_selector_keys) == 1
                    ?? %( $selector_tree{ $matching_selector_keys[0] } )
                    !! %(
                        $matching_selector_keys.map(
                            { %( $selector_tree{$_} ); }
                        )
                    );

                    @queue.push( [ $selector_sub_tree, $data_sub_tree, ] )
                    if $data_sub_tree ~~ Array|Hash &&
                    $selector_sub_tree.keys.grep: { $_ ne '_order_'; };
                }
            }

            if ( $has_includes && !%matched_includes ) {
                %deferred_excludes{$data_tree.WHICH}{$_} = $data_tree
                for @data_tree_keys;
            }

            for keys %deferred_excludes -> $data_tree_string {
                my @data_tree_keys = %deferred_excludes{$data_tree_string}.keys;
                if (@data_tree_keys) {
                    my $data_tree =
                    %deferred_excludes{$data_tree_string}{ @data_tree_keys[0] };
                    next unless $data_tree ~~ Hash|Array;

                    if ( $data_tree ~~ Hash ) {
                        $data_tree{@data_tree_keys}:delete;
                    }
                    else {
                        for @data_tree_keys {
                            %arrays_to_be_trimmed{$data_tree.WHICH} = $data_tree
                              if try $data_tree[$_] = '_to_be_trimmed_';
                        }
                    }
                }
            }

            for values %arrays_to_be_trimmed -> $array {
                @($array) =
                map { $_ eq '_to_be_trimmed_' ?? () !! $_; }, @($array);
            }
        }

        return;
    }
}
