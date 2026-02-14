import type { BangumiKind } from "@/client/types.gen";

interface SeasonTagProps {
  season: number | undefined;
  kind: BangumiKind;
}

export function SeasonTag({ season, kind }: SeasonTagProps) {
  if (kind !== "tv" || !season || season <= 1) return null;

  return (
    <span className="ml-1 font-normal opacity-80">
      第{season}季
    </span>
  );
}
