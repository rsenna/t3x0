! T3X/0 module: shell sort
! Nils M Holm, 2022
! In the public domain / 0BSD License

module shellsrt;

 public sort(v, k, p) do var gap, i, j, tmp;
	gap := k / 2;
	while (gap > 0) do
		i := gap;
		while (i < k) do
			j := i-gap;
			while (j >= 0) do
				if (call p(v[j], v[j+gap]) < 0)
					leave;
				tmp := v[j];
				v[j] := v[j+gap];
				v[j+gap] := tmp;
				j := j - gap;
			end
			i := i+1;
		end
		gap := gap / 2;
	end
 end

end
